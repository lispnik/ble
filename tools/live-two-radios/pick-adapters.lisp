;;;; Choose the two radios, and print the choice as shell assignments.
;;;;
;;;; Run once, before either side starts. That ordering is not incidental:
;;;; reading a controller's BD_ADDR needs the controller, and the peripheral
;;;; takes its adapter away from the kernel for the whole run -- so asking
;;;; afterwards gets NIL for the very address the client needs to connect to.
;;;;
;;;; Adapters are chosen by bus, never by index. hciN numbering drifts across
;;;; reboots, and on this machine the built-in UART radio sits at hci0 while
;;;; the USB dongles land at 1 and 2 -- an arrangement that has already been
;;;; different and will be again.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(let* ((usb (sort (remove-if-not #'ble:hci-adapter-usb-p (ble:list-hci-adapters))
                  #'< :key #'ble:hci-adapter-index)))
  (when (< (length usb) 2)
    (format *error-output*
            "~&need two USB Bluetooth adapters; found ~D~@[: ~{hci~D~^, ~}~]~%~
             (the built-in UART radio cannot be used for both ends)~%"
            (length usb) (mapcar #'ble:hci-adapter-index usb))
    (sb-ext:exit :code 1))
  (destructuring-bind (peripheral central &rest ignored) usb
    (declare (ignore ignored))
    (unless (ble:hci-adapter-address peripheral)
      (format *error-output*
              "~&could not read hci~D's BD_ADDR -- is it held by another ~
               process, or left down by a previous run?~%"
              (ble:hci-adapter-index peripheral))
      (sb-ext:exit :code 1))
    (format t "PERIPH_DEV=~D~%CENTRAL_DEV=~D~%PEER_MAC=~A~%"
            (ble:hci-adapter-index peripheral)
            (ble:hci-adapter-index central)
            (ble:format-mac (ble:hci-adapter-address peripheral)))
    (format *error-output* "~&peripheral: hci~D ~A~@[ (~A)~]~%central:    hci~D~@[ (~A)~]~%"
            (ble:hci-adapter-index peripheral)
            (ble:format-mac (ble:hci-adapter-address peripheral))
            (ble:hci-adapter-product peripheral)
            (ble:hci-adapter-index central)
            (ble:hci-adapter-product central))))
(sb-ext:exit)
