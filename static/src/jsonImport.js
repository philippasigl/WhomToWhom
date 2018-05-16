"use strict";

jsonImport = (url,data) => { 

  xhr = new XMLHttpRequest();

  if (!xhr) throw 'XMLHttpRequest not supported, cannot load the file.'

  xhr.open('GET', url, true);
  xhr.onreadystatechange = function() {
    if (xhr.readyState === 4) {
        data = JSON.parse(xhr.responseText);

    if (typeof obj == 'array') console.log("is array");
        
    xhr.send();
   }
  }
}