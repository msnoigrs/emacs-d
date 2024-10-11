module.exports = {
  page_media_type: 'print',
  pdf_options: {
    format: 'A4',
    margin: '20mm 20mm',
    printBackground: true,
    headerTemplate: `
    <style>
    section {
    margin: 5mm 15mm;
    font-family: system-ui;
    font-size: 10px;
    color: silver;
    </style>
    <section>
    <span class="date"></span>
    </section>`,
    footerTemplate: `
    <section style="margin: 0 auto;">
    <div>
    Page <span class="pageNumber"></span>
    of <span class="totalPages"></span>
    </div>
    </section>`,
  },
  stylesheet: ['.emacs.d/github-markdown.css'],
  body_class: 'markdown-body',
  css: `.page-break { page-break-after: always; }
  .markdown-body {
  font-size: 14px;
  }
  .markdown-body pre > code { white-space: pre-wrap; }
  .markdown-body pre { background: #eee; }
  .markdown-body pre { padding: 5px; }
  `,
};
