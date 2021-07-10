exports (main)

def main(=> currentProcess) :Int as DeepFrozen:
    traceln(`Current process: $currentProcess`)
    "A \r \n \x00 \u1234"
    '\u1234'
    return 0
