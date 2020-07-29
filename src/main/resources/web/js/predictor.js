const lunarisVariantPredictor = {
    "inputFileNames": {},
    "statuses": {},
    "idsPending": [],
    "fieldNames": [],
    "operators": ["==", "=~", "!=", "!=~"],
    "filterGroupCounter": 0,
    "filterCounter": 0
}

function init() {
    getSchema();
    // addFilterGroup();
}


setInterval(updatePendingStatuses, 700);

function submit() {
    const inputFile = document.getElementById("inputfile").files[0];
    const formData = new FormData();

    formData.append("filter", extractFilterExpression());
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
                addFilterGroup();
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

function countChildrenOfClass(filterGroupsParent, className) {
    let n = 0;
    filterGroupsParent.childNodes.forEach( (childNode) => {
        if (childNode.classList && childNode.classList.contains(className)) {
            n++;
        }
    })
    return n;
}
function getLastChildOfClass(parent, className) {
    let lastGroup = null;
    parent.childNodes.forEach( (childNode) => {
        if (childNode.classList && childNode.classList.contains(className)) {
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

function addFilter(group) {
    const filterNode = document.createElement("span");
    const id = "filter" + lunarisVariantPredictor.filterCounter;
    lunarisVariantPredictor.filterCounter++;
    filterNode.id = id;
    filterNode.classList.add("filter");
    const openParenNode = document.createElement("span");
    openParenNode.innerText = "(";
    openParenNode.classList.add("openParen");
    filterNode.appendChild(openParenNode);
    const fieldSelect = createSelectWithOptions(lunarisVariantPredictor.fieldNames);
    fieldSelect.classList.add("fieldSelect");
    filterNode.appendChild(fieldSelect);
    const operatorSelect = createSelectWithOptions(lunarisVariantPredictor.operators);
    operatorSelect.classList.add("operatorSelect");
    filterNode.appendChild(operatorSelect);
    const valueInput = document.createElement("input");
    valueInput.classList.add("valueInput");
    valueInput.setAttribute("size", "20");
    filterNode.appendChild(valueInput);
    const removeFilterButton = document.createElement("button");
    removeFilterButton.innerHTML = "&ominus;";
    tightenButton(removeFilterButton);
    removeFilterButton.onclick = function() { removeFilter(document.getElementById(id)); }
    filterNode.append(removeFilterButton);
    const closeParenNode = document.createElement("span");
    closeParenNode.innerText = ")";
    closeParenNode.classList.add("closeParen");
    filterNode.appendChild(closeParenNode);
    if(countChildrenOfClass(group, "filter") === 0) {
        const nodeAfter = getLastChildOfClass(group, "addFilterButton");
        group.insertBefore(filterNode, nodeAfter);
    } else {
        const nodeAfter = getLastChildOfClass(group, "filter").nextSibling;
        const operatorNode = document.createElement("span");
        operatorNode.innerText = " OR ";
        operatorNode.classList.add("operator");
        group.insertBefore(operatorNode, nodeAfter);
        group.insertBefore(filterNode, nodeAfter);
    }
}

function removeFilter(filterNode) {
    const filterGroupNode = filterNode.parentNode;
    const previousNode = filterNode.previousSibling;
    const nextNode = filterNode.nextSibling;
    filterNode.remove();
    if(previousNode.classList.contains("openParen") && nextNode.classList.contains("operator")) {
        nextNode.remove();
    } else if(previousNode.classList.contains("operator")) {
        previousNode.remove();
    }
    if(countChildrenOfClass(filterGroupNode, "filter") === 0) {
        removeFilterGroup(filterGroupNode);
    }
}

function addFilterGroup() {
    const buttonNode = document.getElementById("addFilterGroupButton");
    tightenButton(buttonNode);
    const filterGroupsParent = document.getElementById("filterGroups");
    const newFilterGroup = document.createElement("span");
    const id = "filterGroup" + lunarisVariantPredictor.filterGroupCounter;
    lunarisVariantPredictor.filterGroupCounter++;
    newFilterGroup.id = id;
    newFilterGroup.classList.add("filterGroup");
    const openParenNode = document.createElement("span");
    openParenNode.innerText = "(";
    openParenNode.classList.add("openParen");
    newFilterGroup.append(openParenNode);
    addFilter(newFilterGroup);
    const addFilterButton = document.createElement("button");
    addFilterButton.innerHTML = "&oplus;"
    addFilterButton.classList.add("addFilterButton");
    tightenButton(addFilterButton);
    addFilterButton.onclick = function() { addFilter(document.getElementById(id)); }
    newFilterGroup.appendChild(addFilterButton);
    const closeParenNode = document.createElement("span");
    closeParenNode.innerText =  ")";
    closeParenNode.classList.add("closeParen");
    newFilterGroup.appendChild(closeParenNode);
    if(countChildrenOfClass(filterGroupsParent, "filterGroup") === 0) {
        filterGroupsParent.insertBefore(newFilterGroup, buttonNode);
    } else {
        const nodeAfter = getLastChildOfClass(filterGroupsParent, "filterGroup").nextSibling;
        const operatorNode = document.createElement("span");
        operatorNode.innerHTML = "<br>AND ";
        operatorNode.classList.add("operator");
        filterGroupsParent.insertBefore(operatorNode, nodeAfter);
        filterGroupsParent.insertBefore(newFilterGroup, nodeAfter);
    }
}

function removeFilterGroup(filterGroupNode) {
    const previousNode = filterGroupNode.previousSibling;
    const nextNode = filterGroupNode.nextSibling;
    filterGroupNode.remove();
    if(previousNode.classList.contains("openParen")  && nextNode.classList.contains("operator")) {
        nextNode.remove();
    } else if(previousNode.classList.contains("operator")) {
        previousNode.remove();
    }
}

function extractFilterExpression() {
    const filterGroupsParent = document.getElementById("filterGroups");
    const filterGroupNodes = [];
    filterGroupsParent.childNodes.forEach((childNode) => {
        if(childNode.classList && childNode.classList.contains("filterGroup")) {
            filterGroupNodes.push(childNode);
        }
    })
    const filterStringsArray = filterGroupNodes.map((filterGroup) => {
        const filters = [];
        filterGroup.childNodes.forEach((childNode) => {
            if(childNode.classList.contains("filter")) {
                filters.push(childNode);
            }
        })
        return filters.flatMap((filter) => {
            let field;
            let operator;
            let value;
            filter.childNodes.forEach((childNode) => {
                if (childNode.classList.contains("fieldSelect")) {
                    if (childNode.options) {
                        field = childNode.options[childNode.selectedIndex].value;
                    }
                } else if (childNode.classList.contains("operatorSelect")) {
                    if (childNode.options) {
                        operator = childNode.options[childNode.selectedIndex].value;
                    }
                } else if (childNode.classList.contains("valueInput")) {
                    if (childNode.value && childNode.value.length > 0) {
                        value = childNode.value;
                    }
                }
            });
            if (field && operator && value) {
                return ["(`" + field + "` " + operator + " \"" + value + "\")"];
            } else {
                return [];
            }
        });
    });
    return filterStringsArray
        .filter((filterStrings) => filterStrings.length > 0)
        .map((filterStrings) => "(" + filterStrings.join(" OR ") + ")")
        .join(" AND ");
}
