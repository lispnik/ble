(in-package #:ble)

;;; Exports for the live BLE I/O layer -- ffi / hci / advertiser / nus /
;;; hci-conn -- which make up the Linux-only `ble` system.
;;;
;;; Declared here rather than in src/package.lisp so that ble/core exports
;;; exactly the symbols it implements. Loading the portable core on a machine
;;; with no BLE stack should not advertise a scanner that cannot run there.
;;;
;;; The symbols are read in #:ble (see the IN-PACKAGE above), so reading them
;;; interns them; EXPORT then makes them external. This file must load before
;;; the files that define them.

(export '(;; Errors and low-level waiting
          syscall-error
          syscall-error-label
          syscall-error-code
          check-syscall
          fd-readable-p
          ;; Raw HCI socket and controller commands
          open-hci-socket
          close-hci-socket
          hci-socket
          hci-socket-p
          hci-socket-fd
          hci-socket-dev
          send-hci-command
          read-hci-packet
          hci-read-bd-addr
          hci-set-default-phy
          ;; Adapter enumeration. Pick by bus and capability, never by index:
          ;; hciN numbering drifts across reboots.
          list-hci-adapters
          default-hci-dev
          hci-adapter
          hci-adapter-index
          hci-adapter-bus
          hci-adapter-product
          hci-adapter-address
          hci-adapter-usb-p
          hci-adapter-label
          ;; LE scanning and advertising reports
          start-extended-scan
          stop-extended-scan
          start-le-scan
          stop-le-scan
          scan-reports
          discover
          discovered
          discovered-p
          discovered-address
          discovered-addr-type
          discovered-name
          discovered-rssi
          discovered-service-uuids
          reports-from-packet
          adv-report
          adv-report-p
          adv-report-event-type
          adv-report-addr-type
          adv-report-address
          adv-report-data
          adv-report-rssi
          ;; Extended advertising (transmit)
          set-adv-set-random-address
          set-extended-adv-parameters
          set-extended-adv-data
          set-extended-adv-enable
          set-adv-parameters
          set-adv-data
          set-scan-response-data
          set-adv-enable
          +adv-ind+
          +adv-scan-ind+
          +adv-nonconn-ind+
          ;; GATT: UUIDs, characteristics, ATT operations
          uuid16
          uuid-string
          gatt-char
          gatt-char-p
          gatt-char-handle
          gatt-char-properties
          gatt-char-uuid
          gatt-char-uuid-string
          gatt-char-property-names
          find-char-by-uuid
          char-handle-by-uuid
          att-test-channel
          att-test-channel-p
          make-att-test-channel
          att-test-channel-sent
          att-test-channel-sent-pdus
          att-send
          att-recv
          att-request
          att-error-p
          att-exchange-mtu
          att-discover-services
          att-discover-characteristics
          att-discover-descriptors
          att-find-service
          find-service-by-uuid
          gatt-service
          gatt-service-p
          gatt-service-start
          gatt-service-end
          gatt-service-uuid
          gatt-service-uuid-string
          +gatt-primary-service+
          +gatt-secondary-service+
          att-find-cccd
          att-read-value
          att-read-long-value
          att-read-characteristic
          value-may-be-truncated-p
          att-write-value
          att-write-command
          att-subscribe
          att-next-notification
          att-confirm-indication
          att-channel-close
          ;; Nordic UART Service GATT client
          nus-connect
          nus-close
          nus-send
          nus-recv
          nus
          nus-p
          nus-fd
          nus-mtu
          nus-rx-handle
          nus-tx-handle
          nus-cccd-handle
          *att-rx-mtu*
          l2cap-att-connect
          ;; LE connection over an HCI_CHANNEL_USER socket
          nus-connect-hci
          hci-user-att-connect
          open-hci-user-socket
          close-hci-user-socket
          hci-le-create-connection
          hci-conn
          hci-conn-handle
          hci-conn-close
          ;; Giving the adapter back (src/teardown.lisp)
          *open-att-channels*
          register-att-channel
          unregister-att-channel
          close-all-att-channels
          install-adapter-teardown))
