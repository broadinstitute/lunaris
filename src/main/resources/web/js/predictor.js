function responseToStringForDebug(response) {
    return "Status code: " + response.status + "\n" +
        "Status text: " + response.statusText + "\n" +
        response.text();
}

function submit() {
    const inputfile = document.getElementById("inputfile").files[0];
    const formData = new FormData();

    formData.append("inputfile", inputfile);
    fetch("/lunaris/predictor/upload", {method: "POST", body: formData})
    .then((response) => response.text())
        .then((text) => alert(text));
}