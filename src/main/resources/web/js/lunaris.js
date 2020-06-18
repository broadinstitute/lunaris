function requestEditArea() {
    return document.getElementById("requestEditArea");
}

function responseDisplayArea() {
    return document.getElementById("responseDisplayArea");
}

async function submitRequest() {
    responseDisplayArea().value = ""
    await query();
}

async function loadExample1() {
    await loadExample("requestMinimalTsv.json")
}

async function loadExample2() {
    await loadExample("requestMinimalJson.json")
}

async function loadExample3() {
    await loadExample("requestMoreRegions.json")
}

async function loadExample4() {
    await loadExample("requestTypingFields.json")
}

async function loadExample5() {
    await loadExample("requestFilterTsv.json")
}

async function loadExample(fileName) {
    fetch("requests/" + fileName)
        .then(response => response.text())
        .then(data => requestEditArea().value = data);
}

async function* makeTextFileLineIterator(requestUrl, httpInit) {
    const utf8Decoder = new TextDecoder('utf-8');
    const response = await fetch(requestUrl, httpInit);
    const reader = response.body.getReader();
    let { value: chunk, done: readerDone } = await reader.read();
    chunk = chunk ? utf8Decoder.decode(chunk) : '';

    const re = /\n|\r|\r\n/gm;
    let startIndex = 0;
    let result;

    for (;;) {
        let result = re.exec(chunk);
        if (!result) {
            if (readerDone) {
                break;
            }
            let remainder = chunk.substr(startIndex);
            ({ value: chunk, done: readerDone } = await reader.read());
            chunk = remainder + (chunk ? utf8Decoder.decode(chunk) : '');
            startIndex = re.lastIndex = 0;
            continue;
        }
        yield chunk.substring(startIndex, result.index);
        startIndex = re.lastIndex;
    }
    if (startIndex < chunk.length) {
        // last line didn't end in a newline char
        yield chunk.substr(startIndex);
    }
}

function addResponseLine(line) {
    responseDisplayArea().value = responseDisplayArea().value + line
}

async function query() {
    const requestUrl = "query"
    const lunRequest = requestEditArea().value;
    const httpInit = {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: lunRequest
    };
    for await (let line of makeTextFileLineIterator(requestUrl, httpInit)) {
        addResponseLine(line);
    }
}
