const lunarisVariantPredictor = {
    "inputFileNames": {},
    "statuses": {},
    "idsPending": [],
    "fieldNames": [],
    "operators": ["==", "=~"]
}

function init() {
    getSchema();
    // addFilterGroup();
}


setInterval(updatePendingStatuses, 700);

function submit() {
    const inputFile = document.getElementById("inputfile").files[0];
    const formData = new FormData();

    formData.append("inputFile", inputFile);
    fetch("/lunaris/predictor/upload", {method: "POST", body: formData})
        .then((response) => {
            if (!response.ok) {
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

function createSelectWithOptions(options) {
    const select = document.createElement("select");
    options.forEach((option) => {
        select.options.add(new Option(option));
    })
    return select;
}

function addFilter(group) {
    const filterNode = document.createElement("span");
    filterNode.classList.add("filter");
    filterNode.appendChild(document.createTextNode("("));
    const fieldSelect = createSelectWithOptions(lunarisVariantPredictor.fieldNames);
    fieldSelect.classList.add("fieldSelect");
    filterNode.appendChild(fieldSelect);
    const operatorSelect = createSelectWithOptions(lunarisVariantPredictor.operators);
    operatorSelect.classList.add("operatorSelect");
    filterNode.appendChild(operatorSelect);
    const valueInput = document.createElement("input");
    valueInput.setAttribute("size", "20");
    filterNode.appendChild(valueInput);
    filterNode.appendChild(document.createTextNode(")"));
    group.appendChild(filterNode);
}

function countFilterGroups(filterGroupsParent) {
    let n = 0;
    filterGroupsParent.childNodes.forEach( (childNode) => {
        if (childNode.classList && childNode.classList.contains("filterGroup")) {
            n++;
        }
    })
    return n;
}

function getLastFilterGroup(filterGroupsParent) {
    let lastGroup = null;
    filterGroupsParent.childNodes.forEach( (childNode) => {
        if (childNode.classList && childNode.classList.contains("filterGroup")) {
            lastGroup = childNode;
        }
    })
    return lastGroup;
}

function tightenButton(button) {
    button.style.margin = "0px 0px 0px 0px";
    button.style.border = "0px";
    button.style.padding = "0px";
}

function addFilterGroup() {
    const buttonNode = document.getElementById("addFilterGroupButton");
    tightenButton(buttonNode);
    const filterGroupsParent = document.getElementById("filterGroups");
    const newFilterGroup = document.createElement("span");
    newFilterGroup.classList.add("filterGroup");
    addFilter(newFilterGroup);
    if(countFilterGroups(filterGroupsParent) === 0) {
        filterGroupsParent.insertBefore(newFilterGroup, buttonNode);
    } else {
        const nodeAfter = getLastFilterGroup(filterGroupsParent).nextSibling;
        const andNode = document.createTextNode(" && ");
        filterGroupsParent.insertBefore(andNode, nodeAfter);
        filterGroupsParent.insertBefore(newFilterGroup, nodeAfter);
    }
}