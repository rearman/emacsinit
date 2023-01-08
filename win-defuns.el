;; -*- lexical-binding: t; -*-
;; DEFUNS FOR WINDOWS ONLY
(defun unfuck-aveva-license ()
  "Remove the offending xml files when aveva can't find the license."
  (interactive)
  (let ((xml1 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalAcquireInfo.xml")
	(xml2 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalBackEndAcquireInfo.xml"))
    (when (file-exists-p xml1) (delete-file xml1))
    (when (file-exists-p xml2) (delete-file xml2))))
