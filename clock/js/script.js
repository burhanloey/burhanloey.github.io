var alarm = new Audio("clock.mp3");
alarm.loop = true;

function pad(num) {
    if (num < 10) {
        return "0" + num;
    }
    return num.toString();
}

function isTimeToWalk(minutes, seconds) {
    if (minutes === "00" && seconds === "00") {
        return true;
    }
    return false;
}

function walk() {
    alarm.play();

    document.getElementById("walk").style.visibility = "visible";
    document.getElementById("gotit").disabled = false;
}

function updateTime() {
    var now = new Date();
    var hours = pad(now.getHours())
    var minutes = pad(now.getMinutes())
    var seconds = pad(now.getSeconds())

    document.getElementById("clock").innerHTML = hours + ":" + minutes + ":" + seconds;

    if (isTimeToWalk(minutes, seconds)) {
        walk();
    }
}
setInterval(updateTime, 1000);

function stop() {
    alarm.pause();
    alarm.currentTime = 0;

    document.getElementById("walk").style.visibility = "hidden";
    document.getElementById("gotit").disabled = true;
}

window.onload = function() {
    updateTime();
}
