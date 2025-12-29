const qs = document.querySelector.bind(document);
const classSelect = qs("#class-select");
const classListContainer = qs("#class-list");

function addClass() {
    if (!classSelect.value) return;

    const newClass = document.createElement("input");
    input.type = "text";
    input.classList.add("class-input");
    input.value = classSelect.value;
    input.disabled = true;

    classListContainer.append(newClass);
}
