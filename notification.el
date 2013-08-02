;; -*- coding:utf-8; mode:emacs-lisp; lexical-binding: t -*-

(require 'json)

(defconst notification-version "0.0.1")
(defconst notification-process "notification-process")

(defvar notification-service-port 1743
  "emacs 内部で起動する通知サーバのポートを指定する")

(defvar notification-display-time 20
  "通知が表示されている時間を秒で指定する")

(defstruct notification
  date
  title
  description)

;; utiles
(let ((instance nil))
  (defun notification-overlay-instance (&optional start-pos end-pos)
    "通知用のオーバーレイオブジェクトを一つだけ管理する"
    (or instance
        (progn
          (assert start-pos)
          (assert end-pos)
          (setq instance (make-overlay start-pos end-pos))))))

(let ((timer nil))
  (defun notification-run-at-time (time func)
    "以前に動いていたタイマーをキャンセルする"
    (when timer (cancel-timer timer))
    (setq timer (run-at-time time nil func))))

(let ((last-string ""))    ;; TODO: すべて取得出来なかったとき用
  (defun notification-process-filter (process string)
    "外部プロセスからのデータ取得"
    (progn
      (message "%s : %s " (process-name process) string)
      (let ((json-object-type 'plist))
        (condition-case err
            (progn
              (let ((notification (json-read-from-string string)))
                (notification-did-appear (make-notification :date (plist-get notification :date)
                                                            :title (plist-get notification :title)
                                                            :description (plist-get notification :description))))
              (message "json : %s" (json-read-from-string string)))
          (json-readtable-error (message "on error : %s" (error-message-string err)))
          (end-of-file (message "on error : %s" (error-message-string err))))))))
  
(let ((client-processes nil))    ;; 接続中のクライアント一覧
  (defun notification-process-log (server client msg)
    (message "%s : %s : %s" (process-name server) (process-name client) msg)
    (message "%s : %s " client-processes client)
    (unless (memq client client-processes)
      (progn
        (if client-processes
            (setq client-processes (cons client client-processes))
          (setq client-processes (list client)))
        (process-kill-without-query client nil)    ;; emacs を終了するときに、確認しない
        (set-process-filter   client 'notification-process-filter)
        (set-process-sentinel client 'notification-process-sentinel))))
  
  (defun notification-process-sentinel (process string)
    "外部プロセスの監視"
    (progn
      (message "notification-process-sentinel : %s" (processp process))
      (case (process-status process)
            ; run  -- for a process that is running.
            ; stop -- for a process stopped but continuable.
            ; exit -- for a process that has exited.
            ; signal -- for a process that has got a fatal signal.
            ; open -- for a network stream connection that is open.
            ; listen -- for a network stream server that is listening.
            ; closed -- for a network stream connection that is closed.
            ; connect -- when waiting for a non-blocking connection to complete.
            ; failed -- when a non-blocking connection has failed.
            ; nil -- if arg is a process name and no such process exists. PROCESS may be a process, a buffer, the name of a process, or nil, indicating the current buffer's process.
            ('run     (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('stop    (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('exit    (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('signal  (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('open    (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('listen  (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('closed
             (progn
               (message "%s : %s : %s " (process-status process) (process-name process) string)
               (delete-process process)
               (delq process client-processes)))
            ('connect (message "%s : %s : %s " (process-status process) (process-name process) string))
            ('failed  (message "%s : %s : %s " (process-status process) (process-name process) string))
            (nil      (message "%s : %s : %s " (process-status process) (process-name process) string)))))
  )    ;; let

;; 通知サーバの起動
(let ((process (make-network-process :name notification-process
                                     :buffer nil
                                     :service notification-service-port
                                     :type nil    ;; tcp
                                     :family 'ipv4
                                     :filter 'notification-process-filter
                                     :filter-multibyte 't
                                     :sentinel 'notification-process-sentinel
                                     :log 'notification-process-log
                                     :server 't)))
  (process-kill-without-query process nil)    ;; emacs を終了するときに、確認しない
  )

(defun notification-did-appear (notification)
  "通知を表示する"
  (assert (notification-p notification))
  (unless (minibufferp (current-buffer))
    (let ((face 'highlight)
          end-point
          width
          overlay)
      (save-excursion
        (goto-char (window-start))
        (forward-line 3)
        (setq end-point (point))
        (setq width (- (window-width) 10)))    ;; linum の分かなぁ
      (notification-overlay-instance (window-start) end-point)
      (move-overlay (notification-overlay-instance) (window-start) end-point)
      (overlay-put (notification-overlay-instance) 'invisible t)
      (overlay-put (notification-overlay-instance) 'after-string 
                   (format "%s\n%s\n%s\n"
                           (notification-title notification)
                           (notification-description notification)
                           (make-string width ?-)))
      (overlay-put (notification-overlay-instance) 'face face)    ;; face があたらん
      (notification-run-at-time notification-display-time
                                (lambda () (delete-overlay (notification-overlay-instance)))))))

(provide 'notification)
