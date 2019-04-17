;;; my-proxy.el --- proxy settings                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Igarashi Masanao

;; Author: Igarashi Masanao <syoux2@gmail.com>

(setq url-proxy-services
      '(("http" . "172.24.254.5:8080")
        ("https" . "172.24.254.5:8080")))
