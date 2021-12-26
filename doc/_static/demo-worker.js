self.languagePluginUrl = '/_static/pyodide/';
importScripts('/_static/pyodide/pyodide.js');

(async function() {
    await languagePluginLoader;
    await self.pyodide.loadPackage(["Pygments"]);
    self.postMessage({loaded: true})
})();

self.onmessage = async (event) => {
    self.pyodide.globals['code'] = event.data.code;
    self.pyodide.globals['lexer_name'] = event.data.lexer;
    self.pyodide.globals['style_name'] = event.data.style;

    const html = self.pyodide.runPython(`
        import pygments.lexers, pygments.formatters.html

        lexer = pygments.lexers.get_lexer_by_name(lexer_name)
        fmter = pygments.formatters.html.HtmlFormatter(noclasses=True, style=style_name)
        if type(code) == memoryview:
            code = bytes(code)
        pygments.highlight(code, lexer, fmter)
    `);

    self.postMessage({html});
}
