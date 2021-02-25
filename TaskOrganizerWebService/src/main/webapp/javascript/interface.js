function addTask(title, type, description, expiration, creator){

    var currentDiv = document.getElementById("backlog-stage");
    var newTaskDiv = document.createElement("div")
    newTaskDiv.className = "task_div";
    var today = new Date();
    var compare_date = new Date(expiration);
    if (compare_date >= today){
        newTaskDiv.style.color = "green";
    }
    else {
        newTaskDiv.style.color = "red";
    }

    //title
    var newTaskTitle = document.createElement("h1");
    newTaskTitle.className = "title";
    var TaskTitle = document.createTextNode(title);
    newTaskTitle.appendChild(TaskTitle);
    newTaskDiv.appendChild(newTaskTitle);

    //type
    var newTaskType = document.createElement("h2");
    newTaskType.className = "type";
    var TaskType = document.createTextNode(type);
    newTaskType.appendChild(TaskType);
    newTaskDiv.appendChild(newTaskType);

    //description
    var newTaskDesc = document.createElement("p");
    newTaskDesc.className = "desc";
    var TaskDesc = document.createTextNode(description);
    newTaskDesc.appendChild(TaskDesc);
    newTaskDiv.appendChild(newTaskDesc);

    //creator
    var newTaskCreator = document.createElement("p");
    newTaskCreator.className = "created";
    var TaskCreator = document.createTextNode("Created by: "+creator);
    newTaskCreator.appendChild(TaskCreator);
    newTaskDiv.appendChild(newTaskCreator);

    //expiration
    var newTaskExp = document.createElement("p");
    newTaskExp.className = "expiration";
    var TaskExp = document.createTextNode("Expires on: "+expiration);
    newTaskExp.appendChild(TaskExp);
    newTaskDiv.appendChild(newTaskExp);

    newTaskDiv.appendChild(currentDiv);
}

function deleteTask(title,stage){
    var tasks;
    if (stage == 0){
        tasks = document.getElementById('backlog-stage').childNodes;
    }
    if (stage == 1){
        tasks = document.getElementById('done-stage').childNodes;
    }
    if (stage == 2){
        tasks = document.getElementById('quality-check-stage').childNodes;
    }
    if (stage == 3){
        tasks = document.getElementById('done-stage').childNodes;
    }
    for (let i = 0; i < tasks.length; i++){
        if (tasks[i].childNodes[0].innerText == title){
            var task_to_delete =  tasks[i];
            if(stage == 0) {
                document.getElementById('backlog-stage').removeChild(task_to_delete);
            }
            if(stage == 1) {
                document.getElementById('doing-stage').removeChild(task_to_delete);
            }
            if(stage == 2) {
                document.getElementById('quality-check-stage').removeChild(task_to_delete);
            }
            if(stage == 3) {
                document.getElementById('done-stage').removeChild(task_to_delete);
            }
            return true;
        }
    }
    return false;
}

function moveTask(title, from_stage, to_stage){

    var task_cln;
    var tasks;
    var currentStage;

    if (stage == 0){
        tasks = document.getElementById('backlog-stage').childNodes;
        currentStage = document.getElementById("backlog-stage");
    }
    if (stage == 1){
        tasks = document.getElementById('done-stage').childNodes;
        currentStage = document.getElementById("done-stage");
    }
    if (stage == 2){
        tasks = document.getElementById('quality-check-stage').childNodes;
        currentStage = document.getElementById("quality-check-stage");
    }
    if (stage == 3){
        tasks = document.getElementById('done-stage').childNodes;
        currentStage = document.getElementById("done-stage");
    }

    for (let i = 0; i < tasks.length; i++){

        if (tasks[i].childNodes[0].innerText == title){

            var task_to_delete =  tasks[i];
            task_cln = task_to_delete.cloneNode(true);

            if(stage == 0) {
                document.getElementById('backlog-stage').removeChild(task_to_delete);
            }
            if(stage == 1) {
                document.getElementById('doing-stage').removeChild(task_to_delete);
            }
            if(stage == 2) {
                document.getElementById('quality-check-stage').removeChild(task_to_delete);
            }
            if(stage == 3) {
                document.getElementById('done-stage').removeChild(task_to_delete);
            }
            currentStage.appendChild(task_cln);
            return true;
        }
    }
    return false;
}