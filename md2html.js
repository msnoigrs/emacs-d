module.exports = {
  stylesheet: ['.emacs.d/github-markdown.css'],
  body_class: 'markdown-body',
  css: `.markdown-body {
  box-sizing: border-box;
  min-width: 200px;
  max-width: 980px;
  margin: 0 auto;
  padding: 45px;
  }

  @media (max-width: 767px) {
  .markdown-body {
  padding: 15px;
  }
  }`,
};