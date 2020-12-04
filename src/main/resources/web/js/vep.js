const lunarisVariantPredictor = {
    inputFileNames: {},
    statuses: {},
    idsPending: [],
    fieldNames: [],
    stringOperators: ["==", "=~", "!=", "!=~"],
    numericalOperators: ["<", "<=", ">", ">="],
    operators: ["==", "=~", "!=", "!=~", "<", "<=", ">", ">="],
    filterGroupCounter: 0,
    filterCounter: 0,
    filters: [],
    masksList: []
}

const codeMirrorConfig = {
    theme: "darcula",
    lineNumbers: true
}

let codeMirror;

function init() {
    getSchema();
    initMasksSelector();
    const codeMirrorParent = document.getElementById("code_mirror_parent");
    codeMirror = CodeMirror(codeMirrorParent, codeMirrorConfig);
    codeMirror.setSize("95%", "7.5em");
}


setInterval(updatePendingStatuses, 300);

function submit() {
    const inputFile = document.getElementById("inputfile").files[0];

    addTemporaryStatus(inputFile);

    const formData = new FormData();

    formData.append("filter", codeMirror.getValue());
    formData.append("inputFile", inputFile);
    fetch("/lunaris/predictor/upload", {method: "POST", body: formData})
        .then((response) => {
            removeTemporaryStatus();
            if (!response.ok) {
                throw "Could not submit " + inputFile.name + ": " + response.statusText;
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

const tempStatusNodeId = "tempStatusNode";

function addTemporaryStatus(file) {
    removeTemporaryStatus();
    const statusNode = document.createElement("p");
    statusNode.id = tempStatusNodeId;
    statusNode.innerText = file.name + ": uploading ...";
    const statusAreaNode = getSubmissionAreaNode();
    statusAreaNode.insertAdjacentElement("afterbegin", statusNode);
}

function removeTemporaryStatus() {
    const statusNode = document.getElementById(tempStatusNodeId);
    if (statusNode) {
        const statusAreaNode = getSubmissionAreaNode();
        statusAreaNode.removeChild(statusNode);
    }
}

function temporaryHackToFixDataProblem(colsRaw) {
    const colsFixed = [];
    colsRaw.forEach((colRaw) => {
            if (colRaw.length < 100) {
                colsFixed.push(colRaw);
            } else {
                colRaw.split(" ").forEach((colSplit) => colsFixed.push(colSplit));
            }
        }
    )
    return colsFixed;
}

function getSchema() {
    fetch("/lunaris/predictor/schema")
        .then((response) => {
            return response.json();
        })
        .then((schema) => {
            if (schema.isError) {
                const statusTextNode = getStatusAreaNode();
                statusTextNode.innerText = "Unable to load available fields: " + schema.message;
            }
            if (schema.col_names) {
                lunarisVariantPredictor.fieldNames = temporaryHackToFixDataProblem(schema.col_names);
                const statusTextNode = getStatusAreaNode();
                statusTextNode.innerText = "Loaded " + lunarisVariantPredictor.fieldNames.length + " field names."
            }
        })
}

function getStatusAreaNode() {
    return document.getElementById("status_area");
}

function getSubmissionAreaNode() {
    return document.getElementById("submission_area");
}

function showCouldNotSubmit(message) {
    const pNode = document.createElement("p");
    pNode.innerText = message;
    const statusAreaNode = getSubmissionAreaNode();
    statusAreaNode.append(pNode);
}

function addStatusEntry(inputFileName, id) {
    const divNode = document.createElement("div");
    const pNode = document.createElement("p");
    divNode.appendChild(pNode);
    const statusAreaNode = getSubmissionAreaNode();
    statusAreaNode.insertAdjacentElement("afterbegin", divNode);
    divNode.setAttribute("id", id)
    showInitialStatus(divNode, inputFileName);
}

function showInitialStatus(divNode, inputFileName) {
    const pNode = divNode.getElementsByTagName("p")[0];
    if(pNode) {
        pNode.innerText = "";
        const textNode = document.createTextNode("Submitted " + inputFileName + ", waiting for result.");
        pNode.append(textNode);
    }
}

function getStatus(id) {
    fetch("/lunaris/predictor/status/" + id)
        .then((response) => response.json())
        .then((status) => {
            lunarisVariantPredictor.statuses[id] = status;
            showStatus(id);
        });
}

function soManyErrors(nSnags) {
    if(nSnags === 0) {
        return "No errors";
    } else if(nSnags === 1) {
        return "One error";
    } else {
        return `${nSnags} errors`;
    }
}

function showStatus(id) {
    const divNode = document.getElementById(id);
    const pNode = divNode.getElementsByTagName("p")[0]
    const inputFileName = lunarisVariantPredictor.inputFileNames[id];
    const status = lunarisVariantPredictor.statuses[id];
    if (status) {
        const textNode = document.createTextNode(inputFileName + ": " + status.message);
        pNode.innerText = "";
        pNode.append(textNode);
        if (status.succeeded) {
            const spaceNode = document.createTextNode(" ");
            const linkNode = document.createElement("a");
            linkNode.setAttribute("href", "/lunaris/predictor/results/" + id);
            linkNode.innerText = "Click here to download";
            pNode.append(spaceNode);
            pNode.append(linkNode);
        }
        const snagMessages = status.snagMessages;
        const nSnags = snagMessages.length;
        if(nSnags) {
            const snagNode = document.createTextNode(" " + soManyErrors(nSnags));
            const snagNodeSpan = document.createElement("span");
            snagNodeSpan.style.color = "red";
            snagNodeSpan.appendChild(snagNode)
            pNode.append(snagNodeSpan);
            const snagMessagesClass = "snagMessages";
            if(!divNode.getElementsByClassName(snagMessagesClass).length) {
                const snagsDivNode = document.createElement("div");
                snagsDivNode.innerText = snagMessages.join("\n");
                snagsDivNode.classList.add(snagMessagesClass);
                snagsDivNode.style.height = "5em";
                snagsDivNode.style.width = "95%"
                snagsDivNode.style.margin = "auto";
                snagsDivNode.style.overflowY = "scroll";
                snagsDivNode.style.color = "red";
                divNode.appendChild(snagsDivNode);
            }
        }
    } else {
        showInitialStatus(divNode, inputFileName);
    }
}

function updatePendingStatuses() {
    const idsPendingNew = [];
    let i = 0;
    const idsPending = lunarisVariantPredictor.idsPending;
    while (i < idsPending.length) {
        const id = idsPending[i];
        getStatus(id);
        showStatus(id);
        const status = lunarisVariantPredictor.statuses[id];
        if (!status.completed) {
            idsPendingNew.push(id);
        }
        i++;
    }
    lunarisVariantPredictor.idsPending = idsPendingNew;
}

function setOptionsForSelect(selectNode, options) {
    options.forEach((option) => {
        selectNode.options.add(new Option(option));
    })
}

function clearFilters() {
    codeMirror.setValue("");
}

function resetFilters() {
    clearFilters();
}

function initMasksSelector() {
    fetch("/lunaris/predictor/masks/list")
        .then((response) => response.json())
        .then((masksList) => {
            lunarisVariantPredictor.masksList = masksList;
            const selectNode = getMaskSelectNode();
            setOptionsForSelect(selectNode, lunarisVariantPredictor.masksList);
        });
}

function getMaskSelectNode() {
    return document.getElementById("masks");
}

function setMask() {
    const maskSelectNode = getMaskSelectNode();
    const maskName = maskSelectNode.value;
    fetch("/lunaris/predictor/masks/" + maskName)
        .then((response) => response.text())
        .then((mask) => {
            codeMirror.setValue(mask);
        });
}