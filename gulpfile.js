const { src, dest, series } = require("gulp");
const pug = require("gulp-pug");
const originalPug = require("pug");
const markdownToJSON = require("gulp-markdown-to-json");
const marked = require("marked");
const through2 = require("through2");

const outputDirectory = "./dist";
function pages() {
  return src("./pages/*.pug")
    .pipe(
      pug({
        // Your options in here.
      })
    )
    .pipe(dest("./dist"));
}

function notes() {
  const compileTemplate = originalPug.compileFile("./notes/template.pug");
  return src("./notes/*.md")
    .pipe(
      markdownToJSON((content) => {
        return marked.parse(content);
      })
    )
    .pipe(
      through2.obj(function (input, _, cb) {
        const json = JSON.parse(input.contents);
        const html = compileTemplate(json);
        input.contents = Buffer.from(html);
        input.extname = ".html";
        this.push(input);
        cb();
      })
    )
    .pipe(dest(outputDirectory + "/notes"));
}

function copy() {
  return src(["./style.css", "./app.js", "./media/**/*"], { base: "." }).pipe(
    dest("./dist")
  );
}

const build = series(copy, pages, notes);

exports.default = build;
