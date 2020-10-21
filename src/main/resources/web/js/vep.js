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

class Filter {
    constructor(field, op, value) {
        this.field = field;
        this.op = op;
        this.value = value;
    }

    applyToNode(filterNode) {
        filterNode.childNodes.forEach((childNode) => {
            if (childNode.classList.contains("fieldSelect")) {
                childNode.value = this.field;
            } else if (childNode.classList.contains("operatorSelect")) {
                childNode.value = this.op;
            } else if (childNode.classList.contains("valueInput")) {
                childNode.value = this.value;
            }
        });
    }
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


setInterval(updatePendingStatuses, 700);

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
    statusAreaNode.appendChild(statusNode);
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
    const pNode = document.createElement("p");
    const statusAreaNode = getSubmissionAreaNode();
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
    } else {
        showInitialStatus(pNode, inputFileName);
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

function createSelectWithOptions(options) {
    const select = document.createElement("select");
    setOptionsForSelect(select, options);
    return select;
}

function countChildrenOfClass(parent, className) {
    let n = 0;
    parent.childNodes.forEach((childNode) => {
        if (childNode.classList && childNode.classList.contains(className)) {
            n++;
        }
    })
    return n;
}

function getAllChildrenOfClass(parent, className) {
    let childrenOfClass = [];
    parent.childNodes.forEach((childNode) => {
        if (childNode.classList && childNode.classList.contains(className)) {
            childrenOfClass.push(childNode);
        }
    })
    return childrenOfClass;
}

function getLastChildOfClass(parent, className) {
    let lastChild = null;
    parent.childNodes.forEach((childNode) => {
        if (childNode.classList && childNode.classList.contains(className)) {
            lastChild = childNode;
        }
    })
    return lastChild;
}

function tightenButton(button) {
    button.style.margin = "0px 0px 0px 0px";
    button.style.border = "0px";
    button.style.padding = "0px";
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