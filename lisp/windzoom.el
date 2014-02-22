;;; Code:
;;; windzoom.el --- Window manipulation with continuous visual feeback
;;                 or real world killer application of zooming interface
;;
;; Copyright (C) 2003 Masatake YAMATO
;;
;; Author: Masatake YAMATO <jet@gyve.org>
;; Created: Thu May  1 19:55:05 2003
;; Keywords: window, visual feeback, zooming, focus
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This program is for our wedding memorial and her birthday.

;;; Code:
(require 'cl)

(defgroup windzoom nil
  "Window manipulation with continuous visual feeback."
  :prefix "windzoom-"
  :group  'windows
  :group  'convenience)

(defcustom windzoom-sit-for-interval 0
  "Value passed to sit-for during zooming."
  :type 'number
  :group 'windzoom)

(define-key ctl-x-map "0" 'windzoom-focus-out)
(define-key ctl-x-map "1" 'windzoom-focus-in)
(define-key ctl-x-map "2" 'windzoom-split-window-vertically)
(define-key ctl-x-map "3" 'windzoom-split-window-horizontally)

(defvar windzoom-window-configurations-stack nil)

(defun windzoom-focus-in (&optional undo)
  (interactive "P")
  (if undo
      (windzoom-undo)
    (setq windzoom-window-configurations-stack nil)
    (let ((window-min-height 1)
          (window-min-width 1)
          (verror nil)
          (herror nil))
      (while (not (one-window-p))
        (unless verror
          (condition-case nil
              (call-interactively 'enlarge-window)
            (error (setq verror t))))
        (unless herror
          (condition-case nil
              (call-interactively 'enlarge-window-horizontally)
            (error (setq herror t))))
        (sit-for windzoom-sit-for-interval)
        (push (current-window-configuration) windzoom-window-configurations-stack)))))

(defun windzoom-focus-out (undo)
  (interactive "P")
  (if undo
      (windzoom-undo)
    (setq windzoom-window-configurations-stack nil)
    (let* ((window-min-height 1)
           (window-min-width 1)
           (window (selected-window))
           (verror nil)
           (herror nil)
           (edge (window-edges window))
           (width (- (nth 2 edge) (nth 0 edge)))
           (height (- (nth 3 edge) (nth 1 edge))))
      (while (and (not (one-window-p)) (window-live-p window))
        (if (eq (windzoom-calc-shrink-direction) 'vertical)
            (unless verror
              (condition-case nil
                  (if (> (window-height window) window-min-height)
                      (call-interactively 'shrink-window)
                    (setq verror t))
                (error (setq verror t))))
          (unless herror
            (condition-case nil
                (if (> (window-width window) window-min-width)
                    (call-interactively 'shrink-window-horizontally)
                  (setq herror t))
              (error (setq herror t)))))
        (if (or herror verror)
        (delete-window window))
        (sit-for windzoom-sit-for-interval)
        (push (current-window-configuration) windzoom-window-configurations-stack)))))

(defun windzoom-undo ()
  (interactive)
  (let ((window-min-height 1)
        (window-min-width 1))
    (while windzoom-window-configurations-stack
      (set-window-configuration (pop windzoom-window-configurations-stack))
      (sit-for windzoom-sit-for-interval))
    windzoom-window-configurations-stack nil))

(defun windzoom-calc-shrink-direction ()
  (unless (one-window-p)
    (let ((win (selected-window))
          (w0 (window-width))
          (h0 (window-height))
          w1 h1)
      (save-window-excursion
        (other-window 1)
        (setq w1 (window-width)
              h1 (window-height)))
      (if (eq w1 w0)
          'vertical
        'horizontal))))

(defun windzoom-calc-split-vertical-parent (owin oheight)
  (save-window-excursion
    (shrink-window 1)
    (if (< (window-height owin) oheight)
        'self
      'other)))

(defun windzoom-calc-split-horizontal-parent (owin owidth)
  (save-window-excursion
    (shrink-window-horizontally 1)
    (if (< (window-width owin) owidth)
        'self
      'other)))

(defun windzoom-split-window-vertically (undo)
  (interactive "P")
  (if undo
      (windzoom-undo)
    (setq windzoom-window-configurations-stack nil)
    (let* ((window-min-height 1)
           nheight 
           nwin
           (owin (selected-window))
           oheight
           parent
           local-stack)
      (push (current-window-configuration) windzoom-window-configurations-stack)
      (save-window-excursion
        (select-window (split-window))
        (setq oheight (window-height owin))
        (setq nwin    (selected-window))
        (push (current-window-configuration) local-stack)
        (setq nheight (window-height nwin))
        (setq parent (windzoom-calc-split-vertical-parent owin oheight))
        (if (eq parent 'self)
            (while (< window-min-height (window-height nwin))
              (shrink-window 1)
              (select-window owin)
              (push (current-window-configuration) local-stack)
              (select-window nwin))
          (select-window owin)
          (while (< window-min-height (window-height nwin))
            (enlarge-window 1)
            (push (current-window-configuration) local-stack))))
      (while local-stack
        (set-window-configuration (pop local-stack))
        (push (current-window-configuration) windzoom-window-configurations-stack)
        (sit-for windzoom-sit-for-interval))
      (select-window owin)
      )))

(defun windzoom-split-window-horizontally (undo)
  (interactive "P")
  (if undo
      (windzoom-undo)
    (setq windzoom-window-configurations-stack nil)
    (let* ((window-min-width 1)
           nwidth
           nwin
           (owin (selected-window))
           owidth
           parent
           local-stack)
      (push (current-window-configuration) windzoom-window-configurations-stack)
      (save-window-excursion
        (select-window (split-window-horizontally))
        (setq owidth (window-width owin))
        (setq nwin    (selected-window))
        (push (current-window-configuration) local-stack)
        (setq nwidth (window-width nwin))
        (setq parent (windzoom-calc-split-horizontal-parent owin owidth))
        (if (eq parent 'self)
            (while (< window-min-width (window-width nwin))
              (shrink-window-horizontally 1)
              (select-window nwin)
              (push (current-window-configuration) local-stack)
              (select-window owin))
          (select-window owin)
          (while (< window-min-width (window-width nwin))
            (enlarge-window-horizontally 1)
            (push (current-window-configuration) local-stack))))
      (while local-stack
        (set-window-configuration (pop local-stack))
        (push (current-window-configuration) windzoom-window-configurations-stack)
        (sit-for windzoom-sit-for-interval))
      (select-window owin))))

(provide 'windzoom)
;;; windzoom.el ends here
