
(defclass pm-polymode-multi-auto (pm-polymode-multi)
  ((auto-innermode
    :initarg :auto-innermode
    :type symbol
    :custom symbol
    :documentation
    "Name of pm-hbtchunkmode-auto object (a symbol). At run time
     this object is cloned and placed in -innermodes of the
     pm-config object."))

  "Configuration for a polymode that allows multiple innermodes
that are not known in advance. Examples are org-mode and markdown.")


(defmethod pm-get-span ((config pm-polymode-multi-auto) &optional pos)
  (message "`pm-polymode-multi-auto' object is obsolete. Please use `pm-polymode' directly." )
  (let ((span-other (call-next-method))
        (proto (symbol-value (oref config :auto-innermode))))
    (if (oref proto :head-reg)
        (let ((span (pm--span-at-point (oref proto :head-reg)
                                       (oref proto :tail-reg)
                                       pos)))
          (if (and span-other
                   (or (> (nth 1 span-other) (nth 1 span))
                       (< (nth 2 span-other) (nth 2 span))))
              ;; treat intersections with the host mode
              (if (car span-other)
                  span-other            ;not host
                ;; here, car span should better be nil; no explicit check
                (setcar (cdr span-other) (max (nth 1 span-other) (nth 1 span)))
                (setcar (cddr span-other) (min (nth 2 span-other) (nth 2 span)))
                span-other)
            (append span (list (pm--get-multi-chunk config span)))))
      span-other)))


(provide 'polymode-obsolete)
