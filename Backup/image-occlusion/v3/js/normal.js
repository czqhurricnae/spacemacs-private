/* Do not remove
GPL 3.0 License

Copyright (c) 2020 Mani

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/* Download */
async function createNormalCloze() {
    // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(481 0.8470338983050847 . 0.18042328042328043)]][[JavaScript 高级程序设计 第四版.pdf: Page 481; Quoting: children 属性]]
    // 注意和childNodes 的区别
    // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(428 0.6900085397096498 . 0.16844349680170576)]][[JavaScript 高级程序设计 第四版.pdf: Page 428; Quoting: 每个节点都有一个 childNodes 属性，其中包含一个 NodeList 的实例。]]
    var child = document.getElementById("SVG101").childNodes;
    document.getElementById("uploadPreview").style.transform = "scale(1)";
    document.getElementById("SVG101").style.transform = "scale(1)";
    var origImage = document.getElementById("uploadPreview").outerHTML;

    var oneTime = true;
    var csvLine = "";

    for (i = 0; i < child.length; i++) {

        var origSVG = "";
        var queSvg = "";
        var ansSvg = "";
        // don't add svg with 0 width and 0 height
        if (child[i].getBBox().height != 0 && child[i].getBBox().width != 0) {

            for (j = 0; j < child.length; j++) {

                // text
                // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(439 0.32161016949152543 . 0.308994708994709)]][[JavaScript 高级程序设计 第四版.pdf: Page 439; Quoting: tagName]]
                if (child[j].tagName == "text") {

                    // get bounding box of text, create and use rectangle as mask

                    var textChild = child[j];

                    var bb = textChild.getBBox();
                    var x = bb.x;
                    var y = bb.y;
                    var w = bb.width;
                    var h = bb.height;

                    origSVG += textChild.outerHTML;

                    if (i == j) {
                        queSvg += '<rect width="' + w + '" height="'+ h +'" fill="'+ questionColor +'" x="' + x + '" y="'+ y +'"></rect>';
                        ansSvg += textChild.outerHTML;
                    } else {
                        queSvg += '<rect width="' + w + '" height="'+ h +'" fill="'+ originalColor +'" x="' + x + '" y="'+ y +'"></rect>';
                        ansSvg += '<rect width="' + w + '" height="'+ h +'" fill="'+ originalColor +'" x="' + x + '" y="'+ y +'"></rect>';
                    }
                }

                // rect, ellipse, polygon
                if (child[j].tagName == "rect" || child[j].tagName == "polygon"
                    || child[j].tagName == "ellipse") {

                    child[j].style.fill = originalColor;

                    origSVG += child[j].outerHTML;

                    if (i == j) {

                        child[j].style.fill = questionColor;

                        queSvg += child[j].outerHTML;

                        child[j].style.fill = originalColor;

                    } else {

                        queSvg += child[j].outerHTML;
                        ansSvg += child[j].outerHTML;
                    }
                }
            }

            // add time stamp
            var timeStamp = new Date().getTime();

            if (child[i].tagName == "rect" || child[i].tagName == "polygon"
                || child[i].tagName == "ellipse" || (child[i].tagName == "text")) {

                // if (oneTime) {
                //     // Origin mask
                //     var origFileName = "cordova-img-occ-orig-" + timeStamp;
                //     // console.log("orig " + origSVG);
                //     origSVG = await saveSVG(origFileName, origSVG, imgHeight, imgWidth);
                //     oneTime = false;
                // }

                // Origin mask
                var origFileName = "cordova-img-occ-orig-" + timeStamp;
                // console.log("orig " + origSVG);

                origSVG = await saveSVG(origFileName, origSVG, imgHeight, imgWidth);

                // Question Mask
                var quesFileName = "cordova-img-occ-ques-" + timeStamp;
                // console.log("Ques " + queSvg);

                queSvg = await saveSVG(quesFileName, queSvg, imgHeight, imgWidth);

                // Answer mask
                var ansFileName = "cordova-img-occ-ans-" + timeStamp;
                // console.log("ans " + ansSvg);

                ansSvg = await saveSVG(ansFileName, ansSvg, imgHeight, imgWidth);

                // get all input note from form
                getNoteFromForm();

                var noteId = "cordova-img-occ-note-" + timeStamp;

                csvLine = noteId +
                    "\t" + noteHeader +
                    "\t" + "<img src='" + originalImageName + "'></img>" +
                    "\t" + "<img src='" + quesFileName + ".svg'></img>" +
                    "\t" + noteFooter +
                    "\t" + noteRemarks +
                    "\t" + noteSources +
                    "\t" + noteExtra1 +
                    "\t" + noteExtra2 +
                    "\t" + "<img src='" + ansFileName + ".svg'></img>" +
                    "\t" + "<img src='" + origFileName + ".svg'></img>" + "\n";

                addCsvLineToViewNote(csvLine);

                var params = {
                    "note": {
                        "deckName": localStorage.getItem("deckName"),
                        "modelName": "Image Occlusion Enhanced",
                        "fields": {
                            "id (hidden)": noteId,
                            "Header": noteHeader || localStorage.getItem("front"),
                            "Image": origImage,
                            "Question Mask": queSvg,
                            "Footer": noteFooter,
                            "Remarks": noteRemarks,
                            "Sources": noteSources,
                            "Extra 1": noteExtra1,
                            "Extra 2": noteExtra2,
                            "Answer Mask": ansSvg,
                            "Original Mask": origSVG,
                        },
                        "options": {
                            "allowDuplicate": true
                        }
                    }
                };

                const result = invoke("addNote", 6, params);
                result.then(
                    (v) => {alert(v)},
                    (e) => {alert(e);},
                );
            }
        }
    }
}

function invoke(action, version, params={}) {
    return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.addEventListener("error", () => reject("failed to issue request"));
        xhr.addEventListener("load", () => {
            try {
                const response = JSON.parse(xhr.responseText);
                if (Object.getOwnPropertyNames(response).length != 2) {
                    throw "response has an unexpected number of fields";
                }
                if (!response.hasOwnProperty("error")) {
                    throw "response is missing required error field";
                }
                if (!response.hasOwnProperty("result")) {
                    throw "response is missing required result field";
                }
                if (response.error) {
                    throw response.error;
                }
                resolve(response.result);
            } catch (e) {
                reject(e);
            }
        });

        xhr.open("POST", "http://127.0.0.1:8765");
        // xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        xhr.send(JSON.stringify({action, version, params}));
    });
}
