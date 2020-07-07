function submit() {
    const inputfile = document.getElementById("inputfile").files[0];
    const formData = new FormData();

    formData.append("inputfile", inputfile);
    fetch("/lunaris/predictor/upload", {method: "POST", body: formData});
    alert(file);
}