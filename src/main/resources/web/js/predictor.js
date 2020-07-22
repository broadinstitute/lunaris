
const lunarisVariantPredictor = {
    "inputFileNames": {},
    "statuses": {},
    "idsPending": []
}

setInterval(updatePendingStatuses, 700);

function submit() {
    const inputFile = document.getElementById("inputfile").files[0];
    const formData = new FormData();

    formData.append("inputFile", inputFile);
    fetch("/lunaris/predictor/upload", {method: "POST", body: formData})
        .then((response) => {
            if(!response.ok) {
                throw "Could not submit " + inputFile.name + ": " + response.statusText
            }
            return response.text();
        })
        .then((id) => {
            addStatusEntry(inputFile.name, id);
            lunarisVariantPredictor.inputFileNames[id] = inputFile.name;
            lunarisVariantPredictor.idsPending.push(id);
            getStatus(id);
        }).catch(showCouldNotSubmit);
}

function getSchema() {
    fetch("/lunaris/predictor/schema")
        .then((response) => {
            return response.json();
        })
        .then((schema) => {
            var schemaNode = document.getElementById("schema");
            if(schema.isError) {
                schemaNode.innerText = "Unable to load available fields: " + schema.message;
            }
            if(schema.col_names) {
                schemaNode.innerText = "Available fields: " + schema.col_names.join(", ");
            }
        })
}

function getStatusAreaNode() {
    return document.getElementById("status_area");
}

function showCouldNotSubmit(message) {
    const pNode = document.createElement("p");
    pNode.innerText = message;
    const statusAreaNode = getStatusAreaNode();
    statusAreaNode.append(pNode);
}

function addStatusEntry(inputFileName, id) {
    const pNode = document.createElement("p");
    const statusAreaNode = getStatusAreaNode();
    statusAreaNode.append(pNode);
    pNode.setAttribute("id", id)
    showInitialStatus(pNode, inputFileName);
}

function showInitialStatus(pNode, inputFileName) {
    pNode.innerText = "";
    const textNode = document.createTextNode("Submitted " + inputFileName + ", waiting for result.");
    pNode.append(textNode);
}

function getStatus(id) {
    fetch("/lunaris/predictor/status/" + id)
        .then((response) => response.json())
        .then((status) => {
            lunarisVariantPredictor.statuses[id] = status;
            showStatus(id);
        });
}

function showStatus(id) {
    const pNode = document.getElementById(id);
    const inputFileName = lunarisVariantPredictor.inputFileNames[id];
    const status = lunarisVariantPredictor.statuses[id];
    if(status) {
        const textNode = document.createTextNode(inputFileName + ": " + status.message);
        pNode.innerText = "";
        pNode.append(textNode);
        if(status.succeeded) {
            const spaceNode = document.createTextNode(" ");
            const linkNode = document.createElement("a");
            linkNode.setAttribute("href", "/lunaris/predictor/results/" + id);
            linkNode.innerText = "Click here to download";
            pNode.append(spaceNode);
            pNode.append(linkNode);
        }
    } else {
        showInitialStatus(pNode, inputFileName);
    }
}

function updatePendingStatuses() {
    const idsPendingNew = [];
    let i = 0;
    const idsPending = lunarisVariantPredictor.idsPending;
    while(i < idsPending.length) {
        const id = idsPending[i];
        getStatus(id);
        showStatus(id);
        const status = lunarisVariantPredictor.statuses[id];
        if(!status.completed) {
            idsPendingNew.push(id);
        }
        i++;
    }
    lunarisVariantPredictor.idsPending = idsPendingNew;
}