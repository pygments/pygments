languagePluginLoader.then(() => {
    // pyodide is now ready to use...
    pyodide.loadPackage('Pygments').then(() => {
        pyodide.runPython('import pygments.lexers, pygments.formatters.html, pygments.styles');

        var lexerlist = pyodide.runPython('list(pygments.lexers.get_all_lexers())');
        var sel = document.getElementById("lang");
        for (lex of lexerlist) {
            var opt = document.createElement("option");
            opt.text = lex[0];
            opt.value = lex[1][0];
            sel.add(opt);
        }

        var stylelist = pyodide.runPython('list(pygments.styles.get_all_styles())');
        var sel = document.getElementById("style");
        for (sty of stylelist) {
            if (sty != "default") {
                var opt = document.createElement("option");
                opt.text = sty;
                opt.value = sty;
                sel.add(opt);
            }
        }

        document.getElementById("hlbtn").disabled = false;
        document.getElementById("loading").style.display = "none";
    });
});

function new_file() {
    pyodide.globals['fname'] = document.getElementById("file").files[0].name;
    var alias = pyodide.runPython('pygments.lexers.find_lexer_class_for_filename(fname).aliases[0]');
    var sel = document.getElementById("lang");
    for (var i = 0; i < sel.length; i++) {
        if (sel.options[i].value == alias) {
            sel.selectedIndex = i;
            reset_err_hl();
            break;
        }
    }
}

function reset_err_hl() {
    document.getElementById("aroundlang").style.backgroundColor = null;
}

function highlight() {
    var select = document.getElementById("lang");
    var alias = select.options.item(select.selectedIndex).value

    if (alias == "") {
        document.getElementById("aroundlang").style.backgroundColor = "#ffcccc";
        return;
    }
    pyodide.globals['alias'] = alias;

    var select = document.getElementById("style");
    pyodide.globals['style'] = select.options.item(select.selectedIndex).value;

    pyodide.runPython('lexer = pygments.lexers.get_lexer_by_name(alias)');
    pyodide.runPython('fmter = pygments.formatters.html.HtmlFormatter(noclasses=True, style=style)');

    var file = document.getElementById("file").files[0];
    if (file) {
        file.arrayBuffer().then(function(buf) {
            pyodide.globals['code_mem'] = buf;
            pyodide.runPython('code = bytes(code_mem)');
            highlight_now();
        });
    } else {
        pyodide.globals['code'] = document.getElementById("code").value;
        highlight_now();
    }
}

function highlight_now() {
    var out = document.getElementById("hlcode");
    out.innerHTML = pyodide.runPython('pygments.highlight(code, lexer, fmter)');
    document.location.hash = "#try";
    document.getElementById("hlcodedl").style.display = "block";
}

function download_code() {
    var filename = "highlighted.html";
    var hlcode = document.getElementById("hlcode").innerHTML;
    var blob = new Blob([hlcode], {type: 'text/html'});
    if (window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveBlob(blob, filename);
    }
    else{
        var elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(blob);
        elem.download = filename;
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
        window.URL.revokeObjectURL(elem.href);
    }
}
