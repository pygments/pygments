importScripts('/_static/pyodide/pyodide.js');

async function loadPyodideAndPygments() {
    self.pyodide = await loadPyodide();
    await self.pyodide.loadPackage(["Pygments"]);
    const styles = self.pyodide.runPython(`
        from pygments.formatters.html import HtmlFormatter
        from pygments.styles import STYLE_MAP
        {s: HtmlFormatter(style=s).get_style_defs('.demo-highlight') for s in STYLE_MAP}
    `).toJs();
    self.postMessage({loaded: {styles}})
}
let pyodideReadyPromise = loadPyodideAndPygments();

self.onmessage = async (event) => {
    // Make sure loading is done.
    await pyodideReadyPromise;
    if (event.data.highlight) {
        self.pyodide.globals.set('code', event.data.highlight.code);
        self.pyodide.globals.set('lexer_name', event.data.highlight.lexer);

        self.pyodide.runPython(`
            import pygments.lexers

            lexer = pygments.lexers.get_lexer_by_name(lexer_name)
            if type(code) == memoryview:
                code = bytes(code)
            tokens = lexer.get_tokens(code)
        `);

        const formatter = event.data.highlight.formatter;
        if (formatter == 'html') {

            const html = self.pyodide.runPython(`
                import io
                from pygments.formatters.html import HtmlFormatter

                fmter = HtmlFormatter(cssclass='demo-highlight')
                buf = io.StringIO()
                fmter.format(tokens, buf)
                buf.getvalue()
            `);
            self.postMessage({html});
        } else if (formatter == 'tokens') {
            const tokens = self.pyodide.runPython('list(tokens)').toJs();
            self.postMessage({tokens});
        } else {
            console.warn('unknown formatter:', formatter);
        }
    } else if (event.data.guess_lexer) {
        self.pyodide.globals.set('code', event.data.guess_lexer.code);
        self.pyodide.globals.set('filename', event.data.guess_lexer.filename);
        const lexer = self.pyodide.runPython(`
            import sys
            sys.setrecursionlimit(1000)
            # TODO: remove after upgrading to Pyodide 0.19

            import pygments.lexers
            import pygments.util

            if type(code) == memoryview:
                code = bytes(code)

            if filename:
                lexer = pygments.lexers.guess_lexer_for_filename(filename, code)
            else:
                lexer = pygments.lexers.guess_lexer(code)
            lexer.aliases[0]
        `);
        self.postMessage({lexer});
    } else {
        console.warn('unknown command: expected highlight or guess_lexer but received ', event.data);
    }
}
