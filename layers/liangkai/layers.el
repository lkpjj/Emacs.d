;;; layers.el --- liangkai layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <kevin.scnu@gmail.com>
;; URL: https://github.com/lkpjj/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

(configuration-layer/remove-layers '(chinese))

(configuration-layer/declare-layers '(
                                      liangkai-better-defaults
                                      liangkai-ui
                                      liangkai-misc
                                      zilongshanren-org
                                      ))
