// The `initSqlJs` function is globally provided by all of the main dist files if loaded in the browser.
// We must specify this locateFile function if we are loading a wasm file from anywhere other than the current html page's folder.
config = {
    locateFile: filename => `js/genanki/sql/sql-wasm.wasm`
}

var SQL;
initSqlJs(config).then(function (sql) {
    //Create the database
    SQL = sql;
});

const m = new Model({
    name: MODEL_NAME,
    id: "2156341623643",
    flds: FIELDS,
    css: CSS1,
    req: [
        [0, "all", [0]],
    ],
    tmpls: [
        {
            name: "Card 1",
            qfmt: QFMT1,
            afmt: AFMT1,
        }
    ],
})

const d = new Deck(1347617346765, deckName)
const p = new Package()

function addImageToDeck(fname, blob) {
    p.addMedia(blob, fname);
}

// add note to deck
var addedCount = 0;
function addNoteToDeck() {
    var container = document.getElementById("noteData");

    var textToExport = "";
    // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(472 0.46355932203389827 . 0.19153439153439156)]][[JavaScript 高级程序设计 第四版.pdf: Page 472; Quoting: childElementCount，返回子元素数量（不包含文本节点和注释）]]
    for (i = 0; i < container.childElementCount; i++) {
        // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(481 0.8470338983050847 . 0.18042328042328043)]][[JavaScript 高级程序设计 第四版.pdf: Page 481; Quoting: children 属性]]
        // 注意和childNodes 的区别
        // [[NOTER_PAGE:/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/JavaScript 高级程序设计 第四版.pdf#(428 0.6900085397096498 . 0.16844349680170576)]][[JavaScript 高级程序设计 第四版.pdf: Page 428; Quoting: 每个节点都有一个 childNodes 属性，其中包含一个 NodeList 的实例。]]
        textToExport += container.children[i].value;
    }

    if (textToExport == "") {
        showSnackbar("Add notes to deck first");
        return;
    }

    // console.log(textToExport);

    var lines = textToExport.split("\n");
    for (l of lines) {
        var noteData = l.split("\t");
        // this deck have 11 fields view config.js for more
        if (noteData.length == 11) {
            addedCount++;
            d.addNote(m.note(noteData))
        }
    }
}

// add deck to package and export
function _exportDeck() {
    p.addDeck(d)
    p.writeToFile('Anki-Deck-Export.apkg')
}

function exportDeck() {
    showSnackbar("Wait... deck is exporting");
    addNoteToDeck();
    _exportDeck();
}
