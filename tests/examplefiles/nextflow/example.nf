#!/usr/bin/env nextflow

// Enable typed processes and workflows
nextflow.enable.types = true

// Include definitions from other scripts or from the Nextflow registry
// include { FASTQC } from './modules/fastqc'
// include { BWA_MEM } from 'nf-core/bwa/mem'

////////////////////////////////////////////////////
// Parameters
////////////////////////////////////////////////////

// Pipeline parameters are declared with a name, type, and optional default.
// They can be set on the command line, e.g. `--input samples.csv`
params {
    // Parameters without a default are required
    input: Path

    // Parameters with a default are optional
    min_reads: Integer = 2

    // Boolean parameters default to false
    save_summary: Boolean
}

////////////////////////////////////////////////////
// Record types and functions
////////////////////////////////////////////////////

// A record type specifies the fields that a record must have.
// Records are duck-typed: extra fields are allowed
record Sample {
    id: String
    fastq: Path
}

record QcSample {
    id: String
    fastq: Path
    num_reads: Integer
    gc: Path
    notes: String?      // optional field
}

// Enums define a fixed set of values
enum Strandedness {
    FORWARD,
    REVERSE,
    UNSTRANDED,
}

// Functions can declare parameter types and a return type
def isPassing(sample: QcSample, min_reads: Integer) -> Boolean {
    sample.num_reads >= min_reads
}

////////////////////////////////////////////////////
// Processes
////////////////////////////////////////////////////

// A process runs a script for each set of inputs. Each execution is a
// *task*, which runs in its own work directory, in parallel with other
// tasks, on any supported executor (local, HPC, cloud, Kubernetes)
process COUNT_READS {
    // Directives control how each task is executed
    tag sample.id
    cpus 1
    memory 1.GB
    container 'ubuntu:24.04'

    input:
    // Each input has a name and a type.
    // `Path` inputs (and `Path` fields in records) are staged automatically
    sample: Sample

    output:
    // Outputs are regular values, built with output functions such as
    // `stdout()`, `file()`, `files()`, `env()`, and `eval()`
    record(
        id: sample.id,
        num_reads: stdout().trim().toInteger(),
    )

    script:
    // The script is a string executed by bash. Nextflow variables use `$`,
    // so escape Bash variables with `\$`
    """
    echo \$(( \$(wc -l < ${sample.fastq}) / 4 ))
    """
}

process GC_CONTENT {
    tag id

    input:
    // Records can be destructured into individual inputs
    record(
        id: String,
        fastq: Path
    )

    stage:
    // The stage section customizes how inputs are staged
    stageAs fastq, 'reads.fq'
    env 'SAMPLE_ID', id

    output:
    record(
        id: id,
        gc: file("${id}.gc.txt"),
    )

    topic:
    // Emit values to a *topic* channel, which can be read from anywhere
    eval('bash --version | head -1') >> 'versions'

    script:
    """
    awk 'NR % 4 == 2 { n += length(\$0); gc += gsub(/[GC]/, "") }
         END { print gc / n }' reads.fq > \${SAMPLE_ID}.gc.txt
    """
}

process SUMMARIZE {
    input:
    // Use collection types for multiple files
    reports: Bag<Path>

    output:
    file('summary.txt')

    script:
    """
    cat ${reports.join(' ')} > summary.txt
    """
}

////////////////////////////////////////////////////
// Workflows
////////////////////////////////////////////////////

// A named workflow composes processes and operators, and can be called
// like a process. `Channel` and `Value` are *dataflow types*
workflow QC {
    take:
    samples: Channel<Sample>
    min_reads: Integer

    main:
    // Calling a process with a channel runs a task for each value.
    // Processes return channels of their outputs
    counts_ch = COUNT_READS(samples)
    gc_ch = GC_CONTENT(samples)

    // Join the results by sample ID, keeping every field
    qc_ch = samples
        .join(counts_ch, by: 'id')
        .join(gc_ch, by: 'id')

    passed_ch = qc_ch.filter { s -> isPassing(s, min_reads) }
    failed_ch = qc_ch.filter { s -> !isPassing(s, min_reads) }

    emit:
    // Each output has a name and optional type (unless there is only one)
    passed: Channel<QcSample> = passed_ch
    failed: Channel<QcSample> = failed_ch
}

// The entry workflow is the entrypoint of the pipeline. It is the only
// place where `params` should be used
workflow {
    main:
    // Load the samplesheet as a channel of records
    samples_ch = channel.of(params.input)
        .flatMap { csv -> csv.splitCsv(header: true) }
        .map { row -> record(id: row.id, fastq: file(row.fastq)) }

    qc = QC(samples_ch, params.min_reads)
    qc.failed.view { s -> "Sample ${s.id} has too few reads" }

    // `collect()` gathers all values into a single dataflow value,
    // so SUMMARIZE runs once with all of the reports
    summary = SUMMARIZE(qc.passed.map { s -> s.gc }.collect())

    // Read values from a topic channel
    channel.topic('versions').unique().view()

    publish:
    // Assign channels and values to workflow outputs
    samples = qc.passed
    summary = summary
}

////////////////////////////////////////////////////
// Outputs
////////////////////////////////////////////////////

// The output block declares the pipeline outputs, and how to publish them
// from the work directory to the output directory (`results` by default)
output {
    samples: Channel<QcSample> {
        // Publish files into a custom directory for each value
        path { s -> "gc/${s.id}/" }
        // Save the channel as a samplesheet, including published file paths
        index {
            path 'samples.csv'
            header true
        }
    }

    summary: Path {
        path '.'
        // Publish settings such as `mode` and `enabled` can also be set here
        mode 'copy'
        enabled params.save_summary
    }
}
