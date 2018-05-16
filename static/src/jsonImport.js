"use strict";

var loadFile = function (filePath, done) {
    console.log("load")
    var xhr = new XMLHttpRequest();
    xhr.onload = function () { return done(this.responseText) }
    xhr.open("GET", filePath, true);
    xhr.send();
}

let jsonImport = (url,result) => {
    console.log("import")
    loadFile(url,((responseText) => {
        console.log("resp",responseText)
        result=responseText
    }))
}
