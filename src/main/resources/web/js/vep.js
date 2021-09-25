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
    theme: "liquibyte",
    lineNumbers: true
}

let codeMirror;

function init() {
    initSession();
    getSchema();
    initMasksSelector();
    const codeMirrorParent = document.getElementById("code_mirror_parent");
    codeMirror = CodeMirror(codeMirrorParent, codeMirrorConfig);
    codeMirror.setSize("95%", "7.5em");
}

function fourHexDigits(num) {
    return ("000" + num.toString(16)).substr(-4);
}

function isWellFormedSessionId(id) {
    const sessionIdRegex = /^[0-9a-f]{8}$/;
    return id.match(sessionIdRegex);
}

function initSession() {
    let sessionId;
    const queryParts = window.location.search.substring(1).split("&");
    queryParts.forEach ( queryPart => {
        [key, value] = queryPart.split("=");
        if(key === "session" && isWellFormedSessionId(value)) {
            sessionId = value;
        }
    })
    if(sessionId) {
        loadSession(sessionId);
    } else {
        sessionId =
            fourHexDigits((new Date).getTime() % 65536) + fourHexDigits(Math.floor(Math.random() * 65537));
        setSessionId(sessionId);
        setEmptySubmissionArea();
    }
}

function setSessionId(sessionId) {
    document.getElementById("session_id_area").innerText = sessionId;
    lunarisVariantPredictor.sessionId = sessionId;
}

function loadSession(sessionId) {
    fetch("/lunaris/predictor/session/" + sessionId)
        .then((response) => response.json())
        .then((session) => {
            if(session.error) {
                alert("Error:\n" + session.message);
                window.log(session.report);
            } else if(session.found) {
                setSessionId(sessionId);
                if(session.filter) {
                    setMask(session.filter);
                }
                if(session.format) {
                    setOutputFormat(session.format);
                }
                setEmptySubmissionArea();
                session.jobs.forEach(job => {
                    const id = job.id;
                    const path = job.inputFile;
                    const inputFileName = path.substring(path.lastIndexOf("/") + 1);
                    addStatusEntry(inputFileName, id);
                });
            } else {
                alert("Unknown session " + sessionId +
                    ".\nNote that sessions are only saved when something is submitted.");
            }
        });
}

function promptAndLoadSession() {
    const sessionId = prompt("Please enter session id.");
    if(sessionId) {
        if(isWellFormedSessionId(sessionId)) {
            loadSession(sessionId);
        } else {
            alert(sessionId + " is not a well formed session id.");
        }
    }
}

function setEmptySubmissionArea() {
    const submissionArea = document.getElementById("submission_area");
    submissionArea.innerHTML =
        "<span id=\"statusUpdatesPlaceholder\">(Submission status updates will appear here)</span>";
}

setInterval(updatePendingStatuses, 300);

function isValidEmail(string) {
    if(!string || string.trim().length === 0) {
        return false;
    }
    const parts = string.split("@");
    if(parts.length !== 2) {
        return false;
    }
    const [user, domain] = parts;
    const domainParts = domain.split("\.");
    return user && user.trim().length > 0 && domainParts.length > 1 &&
        domainParts.every(domainPart => domainPart.trim().length > 0);
}

function submit() {
    const inputFile = document.getElementById("inputfile").files[0];
    let email;
    let userDeclinedEmail;
    while(!userDeclinedEmail && !email) {
        userDeclinedEmail = !confirm("Would you like an email when the job completes?");
        if(!userDeclinedEmail) {
            const emailInput =
                prompt("Please enter your email:", lunarisVariantPredictor.email ?? "");
            if(isValidEmail(emailInput)) {
                email = emailInput;
                lunarisVariantPredictor.email = email;
            } else {
                alert(emailInput + " is not a valid email.");
            }
        }
    }

    addTemporaryStatus(inputFile);

    const formData = new FormData();

    formData.append("filter", codeMirror.getValue());
    formData.append("inputFile", inputFile);
    formData.append("format", getOutputFormat());
    formData.append("session", lunarisVariantPredictor.sessionId);
    formData.append("hg", getHg());
    if(email) {
        formData.append("email", email);
    }
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
                lunarisVariantPredictor.fieldNames = schema.col_names;
                const fieldsSelectNode = document.getElementById("fields");
                setOptionsForSelect(fieldsSelectNode, schema.col_names);
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
    lunarisVariantPredictor.inputFileNames[id] = inputFileName;
    lunarisVariantPredictor.idsPending.push(id);
    const divNode = document.createElement("div");
    const pNode = document.createElement("p");
    divNode.appendChild(pNode);
    const statusAreaNode = getSubmissionAreaNode();
    const placeholder = document.getElementById("statusUpdatesPlaceholder");
    if(placeholder) {
        statusAreaNode.removeChild(placeholder);
    }
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
            const outputFile = id + ".tsv"
            linkNode.setAttribute("href", "/lunaris/predictor/results/" + outputFile);
            linkNode.setAttribute("download", outputFile);
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

function getOutputFormatNode() {
    return document.getElementById("formats")
}

function setOutputFormat(format) {
    getOutputFormatNode().value = format;
}

function getOutputFormat() {
    return getOutputFormatNode().value;
}

function getHg() {
    return document.getElementById("hg").value;
}

function setMask(mask) {
    codeMirror.setValue(mask);
}

function setPredefinedMask() {
    const maskSelectNode = getMaskSelectNode();
    const maskName = maskSelectNode.value;
    fetch("/lunaris/predictor/masks/" + maskName)
        .then((response) => response.text())
        .then((mask) => {
            setMask(mask);
        });
}

