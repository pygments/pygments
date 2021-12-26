self.languagePluginUrl = '/_static/pyodide/';
importScripts('/_static/pyodide/pyodide.js');

(async function() {
    await languagePluginLoader;
    await self.pyodide.loadPackage(["Pygments"]);
    self.postMessage({loaded: true})
})();

self.onmessage = async (event) => {
    if (event.data.highlight) {
        self.pyodide.globals['code'] = event.data.highlight.code;
        self.pyodide.globals['lexer_name'] = event.data.highlight.lexer;
        self.pyodide.globals['style_name'] = event.data.highlight.style;

        const html = self.pyodide.runPython(`
            import pygments.lexers, pygments.formatters.html

            lexer = pygments.lexers.get_lexer_by_name(lexer_name)
            fmter = pygments.formatters.html.HtmlFormatter(noclasses=True, style=style_name)
            if type(code) == memoryview:
                code = bytes(code)
            pygments.highlight(code, lexer, fmter)
        `);

        self.postMessage({html});
    } else if (event.data.guess_lexer) {
        self.pyodide.globals['code'] = event.data.guess_lexer.code;
        const lexer = self.pyodide.runPython(`
            import sys
            sys.setrecursionlimit(1000)
            # TODO: remove after upgrading to Pyodide 0.19

            import pygments.lexers
            import pygments.util

            if type(code) == memoryview:
                code = bytes(code)

            pygments.lexers.guess_lexer(code).aliases[0]
        `);
        self.postMessage({lexer});
    } else {
        console.warn('unknown command: expected highlight or guess_lexer but received ', event.data);
    }
}
