// otherwise the button is enabled when refreshing the page
document.getElementById("hlbtn").disabled = true;


const loadingDiv = document.getElementById("loading");
const langSelect = document.getElementById("lang");
const styleSelect = document.getElementById("style");
const formatterSelect = document.getElementById("formatter");
const highlightBtn = document.getElementById("hlbtn");
const outputDiv = document.getElementById("hlcode");
const codeHeader = document.getElementById("code-header");
const copyLink = document.getElementById("copylink");
const style = document.getElementById("css-style");
const textarea = document.getElementById("code");
const uriTooLongMsg = document.getElementById('uri-too-long');

const qvars = Object.fromEntries(new URLSearchParams(window.location.search));
if (qvars.lexer) {
    langSelect.value = qvars.lexer;
}
if (qvars.code !== undefined) {
    textarea.value = qvars.code;
    loadingDiv.hidden = false;
}
if (qvars.style !== undefined) {
    styleSelect.value = qvars.style;
}
if (qvars.formatter !== undefined) {
    formatterSelect.value = qvars.formatter;
}

styleSelect.addEventListener('change', () => {
    style.textContent = styles[styleSelect.value];
    updateCopyLink();
});

let styles;

const highlightWorker = new Worker("/_static/demo-worker.js");
highlightWorker.onmessage = async (msg) => {
    if (msg.data.loaded) {
        styles = msg.data.loaded.styles;
        highlightBtn.disabled = false;
        highlightBtn.textContent = 'Highlight';

        if (qvars.code !== undefined) {
            loadingDiv.hidden = true;
            await highlight();
        }
    } else if (msg.data.html) {
        outputDiv.innerHTML = msg.data.html;
        codeHeader.hidden = false;
        loadingDiv.hidden = true;
        style.textContent = styles[styleSelect.value];
    } else if (msg.data.tokens) {
        const table = document.createElement('table');
        table.className = 'tokens';
        for (const [tokenType, value] of msg.data.tokens) {
            const tr = document.createElement('tr');
            const td1 = document.createElement('td');
            td1.textContent = tokenType.join('.');
            const td2 = document.createElement('td');
            const inlineCode = document.createElement('code');
            inlineCode.textContent = value;
            td2.appendChild(inlineCode);
            tr.appendChild(td1);
            tr.appendChild(td2);
            table.appendChild(tr);
        }
        outputDiv.innerHTML = '';
        outputDiv.appendChild(table);

        codeHeader.hidden = false;
        loadingDiv.hidden = true;
    } else if (msg.data.lexer) {
        await highlight(msg.data.lexer);
    } else {
        console.warn('unexpected message from highlight worker', msg);
    }
};

function updateCopyLink() {
    var url = document.location.origin + document.location.pathname +
        "?" + new URLSearchParams({
            lexer: langSelect.value,
            style: styleSelect.value,
            formatter: formatterSelect.value,
            code: textarea.value,
        }).toString()
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

async function highlight(guessedLexer) {
    var lexer = langSelect.value || guessedLexer;
    var file = document.getElementById("file").files[0];

    let code;
    if (file) {
        code = await file.arrayBuffer();
    } else {
        code = textarea.value;
    }

    outputDiv.innerHTML = '';
    codeHeader.hidden = true;
    loadingDiv.hidden = false;

    if (!lexer) {
        const guess_lexer = {code};
        if (file)
            guess_lexer.filename = file.name;
        highlightWorker.postMessage({guess_lexer});
        document.getElementById('loading-text').textContent = 'guessing lexer...';
        return;
    }

    document.getElementById('loading-text').textContent = 'highlighting code...';

    document.getElementById('guessed-lexer').textContent = guessedLexer;

    highlightWorker.postMessage({highlight: {code, lexer, formatter: formatterSelect.value}});

    if (code instanceof ArrayBuffer) {
        copyLink.hidden = true;
        uriTooLongMsg.hidden = true;
    } else {
        updateCopyLink();
    }
}

copyLink.addEventListener('click', async (e) => {
    e.preventDefault();
    await navigator.clipboard.writeText(e.target.href);
});

function download_code() {
    var filename = "highlighted.html";
    var hlcode = document.getElementById("hlcode").innerHTML + style.outerHTML;
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
