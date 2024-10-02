function mainJob(url, args) {
// http[s]://dict.eudic.net/dicts/en/
if (url.match(/http[s]?:\/\/*dict.eudic.net\/liju\/en\/*/)) {
    if (!$("a.voice-button").length) {
        return false;
    }
    $("a.voice-button").each(function() {
        const audioUrl =
            "http://api.frdic.com/api/v2/speech/speakweb?" +
            $(this).attr("data-rel");
        insertDownloadLink(audioUrl, $(this), true, glossary=args[0] ? args[0][1] : "", notes=args[0] ? args[0][0] : "");
    });
}}

function insertDownloadLink(
    audioUrl,
    insertAfterDOM,
    isInsertLogo = true,
    glossary = "",
    notes = "",
) {
    const imageBase64 = "data:img/jpg;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAADh0lEQVR42uVVW0hUURRdmmkWvSwie4D2MiwmQiebGZ2ZO9eovvqIKKK/wiKKoJCCoIkKioqKoocVBX3FfJRaOc87d2bs+SVSEUQaEZGhFYnRQ8vWOXPhTjNmz78OLJi5+5y99ln7cYD/ZHmzYQ9OgE2bjFJfboa5KrAMivYAzvBqFF0c9pvOfUNQfn0anJHtcGmn4AzNzNhib1wONf4GnlgLKkMOfsn6Nd+22/mwR6xw66fpoAPVic9YlHBJW2HdcMy+Mg6oGSqhaMdQHX8NRT8jb/vTVdE0CpWRJTxwmc47saj5C533JQn0HFQGlpD4EBz6Qu7OhsVXDI9+G554K2+6FFN8+aazsvBo2AIFkrkqUIiF/iKpp0cPQU28Q3XzVxL0mwTeXLgj66DG2qDELmAGg0F/DlzRrdzfQeLdsPttJoErXAslsotOT8CtnZNRexIPKclH03kqAZcjMIfOr5GkA9ZGazLZIReDaoMn2gR36LBJoFJbNdGbdCCkIITj75ynEcCbIyNVE28p5fYkQcRCgpskeAol1JpK0EdnXzIdpiOVgMsZX8uzL6jAWfm/MjKLt/eT5AMV6fl7ArfOPDSTIFQn/yv+EqiaH6reA4/WZRIosceslHYSdVH3bhL1EL2DEkw5kg+nvpfnKFFwa/IGgXmsIiHRE7jDzSaBM1rDaDbRuI/JPQIlXsemuUeyDz8kqPBb4I41MahnJKiQzeXUPEz6E8p0jQXjNQkcDSNRcXcUyvTxskwXBIuZsFXcGGCE3ZllKhIaXs5AHsEdPYtpLHMxQlxaLfe/ZLBe2Brmp7aVaIrFxA5iJ7EeeeUldLLYaLSuZI5kpRkETaUs6Q2whefLWWWtn8ro77CbW2SjlXq/m1kFxFHiHfGJECXmQJE+jKPCJiVT488pWzdcsSrjTDZwPI8NliUbz6kdoFydzOdJWG9MTB8MnCfYSLwm+ohXxBppEZHYg3MZ7W7iPBzRkoyxUnZ1OvPBIokl2GyqvFHaEtNvJdFJfCVEDW8xzSuGoEzkprF4wHFsqS/h6LgPV7AWlksj0s25xg1WGQT9xHtim/FdRJMz6AguJ4ESvoWq62sGMgtNRR37jMj7jTz4je8WI4g/XpOI/UQH0WtIJPLwkjhIjP8XbyKrAXuIx8Rnop04JPr1Xz68hcRmImZIM/XXn77BV0HK7zGEeKHGpu35Y5m+AdqWqWkG1d4JAAAAAElFTkSuQmCC";
    let HTML =
        "<a target='_blank' href='" +
        audioUrl +
        "'>";
    if (isInsertLogo) {
        HTML +=
            "<img src='" +
            imageBase64 +
            "' alt='Download Audio' height='16' width='16'>";
    }
    HTML += "</a>";
    ELEMENT = $(HTML);
    ELEMENT.bind("click", function() {
        const { word, lijuMp3, pronunciationMp3, dataRel, line, exp } = prepareContent($(this)[0]);
        console.log(dataRel);
        const params = {
            "note": {
                "deckName": "English",
                "modelName": "Antimoon",
                "fields": {
                    "expression"    : word || "",
                    "phonetic"      : "",
                    "pronunciation" : "[sound:" + pronunciationMp3 + "]",
                    "glossary"      : glossary || "",
                    "sentence"      : line + " " + exp,
                    "translation"   : exp,
                    "notes"         : notes || "",
                    "url"           : document.URL || "",
                    "image"         : "",
                    "add-dw"        : "1",
                },
                "audio": [{
                    "url": dataRel,
                    "filename": lijuMp3,
                    "fields": [
                        "sound"
                    ]
                }],
                "options": {
            	      "allowDuplicate": true
                }
            }
        };
        const result = invoke("addNote", 6, params);
        result.then(
            (v) => {
                // 例句的单词使用youdao。
                invoke("storeMediaFile", 6, {
                    "url": "http://dict.youdao.com/dictvoice?type=2&audio=" + word,
                    "filename": pronunciationMp3
                });
                alert(v);
            },
            (e) => {alert(e);},
        );
    });
    ELEMENT.insertAfter(insertAfterDOM);
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

function prepareContent(node) {
    let word = decodeURI(document.URL.split("/").pop());
    let dataRel = $(node).attr("href");
    let [, line] = /(?:\d+.)(.+)/.exec(node.parentElement.textContent);
    let exp;
    if (/(?:\d+.)(.+)/.exec(node.parentElement.nextElementSibling.textContent)){
        [, exp] = /(?:\d+.)(.+)/.exec(node.parentElement.nextElementSibling.textContent);
    } else {
        exp = node.parentElement.nextElementSibling.textContent;
    }
    let { lijuMp3, pronunciationMp3 } = formatMp3Name(new Date(), word);
    return({ word, lijuMp3, pronunciationMp3, dataRel, line, exp });
}

function formatMp3Name(date, word) {
    var hours = date.getHours();
    var minutes = date.getMinutes();
    var seconds = date.getSeconds();
    var ampm = hours >= 12 ? 'PM' : 'AM';
    hours = hours % 12;
    hours = hours ? hours : 12; // the hour '0' should be '12'
    minutes = minutes < 10 ? '0'+minutes : minutes;
    var lijuMp3 = 'eudic_' + hours + '_' + minutes + '_' + seconds + '_' + ampm + '_' + word + '.mp3';
    var pronunciationMp3 = 'pronunciation_' + hours + '_' + minutes + '_' + seconds + '_' + ampm + '_' + word + '.mp3';
    return({ lijuMp3, pronunciationMp3 });
}

function playSuccess() {
    const sound = $("<audio id='success'><source src='https://www.soundjay.com/communication/sounds/tape-recorder-close-1.mp3'></audio>")[0];
    sound.play();
}

(function() {
    mainJob(document.URL, arguments);
    Array.from(document.getElementsByClassName('adsbygoogle adsbygoogle-noablate')).forEach(e => { e.style.display = 'none' });
})
