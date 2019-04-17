;;; org-nikola.el --- Export reStructuredText using org-mode.

;; Copyright (C) 2013  IGARASHI Masanao

;; Author: IGARASHI Masanao <syoux2@gmail.com>
;; Keywords: org, nikola, blog
;; Version: 0.1

;;; Commentary:

;;; Code
(require 'ox-publish)
(require 'ox-nikola)
(require 'cl)

;(defun auto-export-my-blog ()
;  (let* ((project-plist (cdr (assoc "blog" org-publish-project-alist)))
;		 (project-dir (expand-file-name
;					   (plist-get project-plist :base-directory))))
;	(save-excursion
;	  (if (string= project-dir (file-name-directory buffer-file-name))
;		  (org-publish-current-file)))))

(defun auto-export-my-blog ()
  (let* ((d (file-name-directory buffer-file-name))
         (project-dir-plist (mapcar
                             #'(lambda (prj)
                                (expand-file-name
                                 (plist-get (cdr prj) :base-directory)))
                             org-publish-project-alist))
         (publishp (loop for x in project-dir-plist
                         when (string= d x) return t)))
	(save-excursion
	  (if publishp
		  (org-publish-current-file)))))

(add-hook 'after-save-hook 'auto-export-my-blog)

(add-to-list 'org-publish-project-alist
			 '("blog" . (:base-directory "~/docs/blog/"
					     :base-extension "org"
						 :publishing-directory "~/nikola/test/posts/"
						 :publishing-function (org-nikola-publish-to-rst))))
;						 :body-only t)))

(add-to-list 'org-publish-project-alist
			 '("stories" . (:base-directory "~/docs/stories/"
					     :base-extension "org"
						 :publishing-directory "~/nikola/test/stories/"
						 :publishing-function (org-nikola-publish-to-rst))))

(add-to-list 'org-publish-project-alist
			 '("idulunews" . (:base-directory "~/products/site/idulu/org/news/"
                         :base-extension "org"
						 :publishing-directory "~/products/site/idulu/news/"
						 :publishing-function (org-nikola-publish-to-rst))))

(add-to-list 'org-publish-project-alist
			 '("iduluposts" . (:base-directory "~/products/site/idulu/org/posts/"
                         :base-extension "org"
						 :publishing-directory "~/products/site/idulu/posts/"
						 :publishing-function (org-nikola-publish-to-rst))))

(add-to-list 'org-publish-project-alist
			 '("iduluroot" . (:base-directory "~/products/site/idulu/org/root/"
                         :base-extension "org"
						 :publishing-directory "~/products/site/idulu/root/"
						 :publishing-function (org-nikola-publish-to-rst))))
