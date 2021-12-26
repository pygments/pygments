// otherwise the button is enabled when refreshing the page
document.getElementById("hlbtn").disabled = true;


const loadingDiv = document.getElementById("loading");
const langSelect = document.getElementById("lang");
const highlightBtn = document.getElementById("hlbtn");
const outputDiv = document.getElementById("hlcode");
const codeHeader = document.getElementById("hlcodedl");
const copyLink = document.getElementById("copylink");

const qvars = getQueryVariables();
if (qvars['lexer']) {
    langSelect.value = qvars['lexer'];
}
if (qvars['code'] !== undefined) {
    document.getElementById("code").value = qvars['code'];
    loadingDiv.hidden = false;
}

const highlightWorker = new Worker("/_static/demo-worker.js");
highlightWorker.onmessage = async (msg) => {
    if (msg.data.loaded) {
        highlightBtn.disabled = false;
        highlightBtn.textContent = 'Highlight';

        if (qvars['code'] !== undefined) {
            loadingDiv.hidden = true;
            await highlight();
        }
    } else if (msg.data.html) {
        outputDiv.innerHTML = msg.data.html;
        codeHeader.hidden = false;
        loadingDiv.hidden = true;
    }
};

function getQueryVariables() {
    var query = window.location.search.substring(1);
    var vars = query.split('&');
    var var_obj = {};
    for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split('=');
	var_obj[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);
    }
    return var_obj;
}

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

async function highlight() {
    var lexer = document.getElementById("lang").value;

    if (lexer == "") {
        document.getElementById("aroundlang").style.backgroundColor = "#ffcccc";
        return;
    }

    var style = document.getElementById("style").value;

    var file = document.getElementById("file").files[0];

    const uriTooLongMsg = document.getElementById('uri-too-long');

    let code;
    if (file) {
        code = await file.arrayBuffer();
        copyLink.hidden = true;
        uriTooLongMsg.hidden = true;
    } else {
        code = document.getElementById("code").value;
        var url = document.location.origin + document.location.pathname +
            "?lexer=" + encodeURIComponent(lexer) + "&code=" + encodeURIComponent(code);
        if (url.length > 8201) {
            // pygments.org is hosted on GitHub pages which does not support URIs longer than 8201
            copyLink.hidden = true;
            uriTooLongMsg.hidden = false;
        } else {
            copyLink.href = url;
            copyLink.textContent = 'Copy link';
            copyLink.hidden = false;
            uriTooLongMsg.hidden = true;
        }
    }

    highlightWorker.postMessage({code, lexer, style});
    outputDiv.innerHTML = '';
    codeHeader.hidden = true;
    loadingDiv.hidden = false;
    document.getElementById('loading-text').textContent = 'highlighting code...';
}

copyLink.addEventListener('click', async (e) => {
    e.preventDefault();
    await navigator.clipboard.writeText(e.target.href);
});

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
