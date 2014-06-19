(let (
        (appdir (sb-posix:getcwd))
        (librootdir (concatenate 'string (sb-posix:getcwd) "../../Sources/Lisp/")))
    (asdf:initialize-source-registry
        `(:source-registry
            (:directory ,appdir)
            (:tree ,librootdir)
            :inherit-configuration))
    (print asdf:*central-registry*))