self.languagePluginUrl = '/_static/pyodide/';
importScripts('/_static/pyodide/pyodide.js');

(async function() {
    await languagePluginLoader;
    await self.pyodide.loadPackage(["Pygments"]);
    const styles = self.pyodide.runPython(`
        from pygments.formatters.html import HtmlFormatter
        from pygments.styles import STYLE_MAP
        {s: HtmlFormatter(style=s).get_style_defs('.demo-highlight') for s in STYLE_MAP}
    `);
    self.postMessage({loaded: {styles}})
})();

self.onmessage = async (event) => {
    if (event.data.highlight) {
        self.pyodide.globals['code'] = event.data.highlight.code;
        self.pyodide.globals['lexer_name'] = event.data.highlight.lexer;

        const html = self.pyodide.runPython(`
            import pygments.lexers, pygments.formatters.html

            lexer = pygments.lexers.get_lexer_by_name(lexer_name)
            fmter = pygments.formatters.html.HtmlFormatter(cssclass='demo-highlight')
            if type(code) == memoryview:
                code = bytes(code)
            pygments.highlight(code, lexer, fmter)
        `);

        self.postMessage({html});
    } else if (event.data.guess_lexer) {
        self.pyodide.globals['code'] = event.data.guess_lexer.code;
        self.pyodide.globals['filename'] = event.data.guess_lexer.filename;
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
