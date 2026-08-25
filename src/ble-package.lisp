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

(export '(;; Conditions (src/att-conditions.lisp; the BLE-ERROR root is
          ;; ble/core's, in src/conditions.lisp)
          att-error
          att-error-code
          att-error-opcode
          att-error-handle
          att-error-name
          att-timeout
          att-timeout-operation
          att-timeout-ms
          peer-disconnected
          peer-disconnected-handle
          gatt-not-found
          gatt-not-found-what
          gatt-not-found-uuid
          *att-signal-errors*
          with-ble-conditions
          ;; Errors and low-level waiting
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
          hci-socket-acl-len
          ;; The outbound flow-control window, so a consumer can see
          ;; backpressure rather than only feel it.
          hci-socket-acl-credits
          hci-socket-acl-max-credits
          *acl-credit-timeout-ms*
          send-hci-command
          *hci-command-timeout-ms*
          hci-command-error
          hci-command-error-opcode
          hci-command-error-status
          hci-command-error-name
          command-answer-status
          command-return-params
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
          ;; The constructor too, for the same reason as
          ;; MAKE-GATT-CHAR: a consumer's filter predicate is
          ;; testable without a radio only if it can be handed
          ;; a DISCOVERED that no radio produced.
          make-discovered
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
          set-random-address
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
          ;; The written form is the reverse of the wire form, so a
          ;; hand-reversed literal is unreviewable. Anyone defining a
          ;; vendor service needs this.
          uuid128
          gatt-char
          ;; The constructor as well as the accessors: a consumer
          ;; testing its own client code needs to build one without
          ;; a radio to discover it from.
          make-gatt-char
          gatt-char-p
          gatt-char-handle
          gatt-char-properties
          gatt-char-uuid
          gatt-char-uuid-string
          gatt-char-property-names
          find-char-by-uuid
          find-chars-by-uuid
          char-handle-by-uuid
          att-test-channel
          att-test-channel-p
          make-att-test-channel
          att-test-channel-sent
          att-test-channel-sent-pdus
          att-test-channel-inbox
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
          +gatt-cccd+
          +gatt-characteristic-decl+
          +gatt-primary-service+
          +gatt-secondary-service+
          att-find-cccd
          att-read-value
          att-read-long-value
          att-read-characteristic
          value-may-be-truncated-p
          att-write-value
          att-read-multiple
          att-prepare-write
          att-execute-write
          att-write-long-value
          att-write-command
          att-subscribe
          att-next-notification
          att-next-notification-any
          att-pending-notifications
          att-clear-notifications
          *att-notification-queue-limit*
          att-confirm-indication
          att-channel-close
          ;; Nordic UART Service GATT client
          nus-connect
          nus-close
          nus-send
          nus-recv
          +nus-service-uuid-le+
          +nus-rx-uuid-le+
          +nus-tx-uuid-le+
          nus
          nus-p
          nus-fd
          nus-mtu
          nus-rx-handle
          nus-tx-handle
          nus-cccd-handle
          *att-rx-mtu*
          att-mtu
          att-forget-mtu
          l2cap-att-connect
          ;; LE connection over an HCI_CHANNEL_USER socket
          nus-connect-hci
          hci-user-att-connect
          open-hci-user-socket
          close-hci-user-socket
          hci-le-create-connection
          hci-conn
          hci-conn-handle
          hci-conn-sock
          hci-conn-close
          hci-pump
          hci-take-event
          hci-drop-events
          hci-conn-events
          ;; GATT server: the attribute database and the half of ATT that
          ;; answers requests (src/gatt-server.lisp)
          make-gatt-server
          gatt-server
          gatt-server-p
          gatt-server-mtu
          gatt-server-rx-mtu
          ;; The subscription state, keyed by CCCD handle. Its
          ;; siblings are all exported, and a peripheral's own
          ;; tests need to stand in for a client that subscribed.
          gatt-server-cccd
          gatt-server-services
          gatt-server-attributes
          gatt-add-service
          gatt-add-characteristic
          gatt-attribute
          gatt-attribute-p
          gatt-attribute-handle
          gatt-attribute-uuid
          gatt-attribute-value
          gatt-attribute-permissions
          gatt-attribute-security
          gatt-server-encrypted
          gatt-attribute-read
          gatt-attribute-on-write
          ;; An ON-WRITE handler returns an ATT error code to refuse
          ;; the write, so a consumer writing one needs a name for at
          ;; least the length check it cannot make the server do.
          +att-err-invalid-value-length+
          gatt-attribute-count
          gatt-find-attribute
          gatt-set-value
          gatt-service-entry
          gatt-service-entry-start
          gatt-service-entry-end
          gatt-service-entry-uuid
          gatt-serve
          gatt-add-descriptor
          *gatt-server-trace*
          *max-prepared-writes*
          +max-attribute-length+
          gatt-serve-pdu
          gatt-notify
          gatt-subscribed-p
          ;; So a peripheral can tell that its indication was
          ;; confirmed and it may send the next one. GATT allows
          ;; exactly one outstanding indication per bearer.
          +att-handle-value-cfm+
          char-properties-bitmap
          ;; Being a peripheral (src/peripheral.lisp)
          peripheral-accept
          serve-peripheral
          make-peripheral-pairing
          peripheral-pairing
          peripheral-pairing-p
          peripheral-pairing-session
          peripheral-pairing-peer
          peripheral-pairing-local-addr
          peripheral-pairing-irk
          ;; Resource-safe wrappers (src/with.lisp)
          with-hci-socket
          with-hci-user-socket
          with-att-channel
          with-nus
          with-nus-hci
          with-extended-scan
          with-advertising
          ;; Established-connection control (src/conn-params.lisp)
          hci-connection-update
          hci-read-remote-features
          hci-read-remote-version
          hci-read-rssi
          ms-to-interval-units
          interval-units-to-ms
          ms-to-timeout-units
          timeout-units-to-ms
          ;; L2CAP signalling: how a peripheral asks for new parameters
          ;; (src/l2cap-signalling.lisp)
          l2cap-request-conn-params
          l2cap-answer-conn-param-request
          l2cap-serve-signalling
          parse-conn-param-request
          *l2cap-accept-conn-param-updates*
          hci-acl-send-l2cap
          hci-conn-sig-pending
          hci-conn-sig-results
          l2cap-conn-param-result
          *l2cap-signalling-handler*
          ;; Connection-oriented channels (src/l2cap-coc.lisp)
          l2cap-coc
          l2cap-coc-p
          l2cap-coc-connect
          l2cap-coc-listen
          l2cap-coc-accept
          l2cap-coc-send
          l2cap-coc-recv
          l2cap-coc-close
          l2cap-coc-give-credits
          l2cap-coc-scid
          l2cap-coc-dcid
          l2cap-coc-mtu
          l2cap-coc-mps
          l2cap-coc-peer-mtu
          l2cap-coc-peer-mps
          l2cap-coc-tx-credits
          l2cap-coc-rx-credits
          l2cap-coc-closed
          *coc-default-mtu*
          *coc-default-mps*
          *coc-default-credits*
          ;; Security Manager (src/smp.lisp, src/smp-crypto.lisp)
          smp-pair
          smp-association-model
          io-capability-code
          passkey-octets
          passkey-bit
          +smp-io-capabilities+
          smp-request-security
          smp-pairing-requested-p
          smp-peer-addr-type
          smp-session
          smp-session-p
          smp-session-ltk
          smp-session-peer-irk
          smp-session-peer-identity-addr
          smp-session-peer-identity-addr-type
          smp-send-identity
          smp-receive-keys
          smp-session-mackey
          smp-start-encryption
          smp-answer-ltk-request
          smp-await-encryption
          smp-error
          smp-error-reason
          smp-error-source
          *smp-trace*
          smp-random
          smp-random-octets
          os-random
          smp-f4
          smp-f5
          smp-f6
          smp-g2
          smp-generate-keypair
          smp-dhkey
          smp-ah
          aes-e
          resolve-address
          generate-rpa
          smp-public-key-valid-p
          aes-cmac
          msb
          ;; Bonds that outlive the process (src/bonds.lisp)
          bond
          bond-p
          make-bond
          bond-identity-addr
          bond-identity-addr-type
          bond-irk
          bond-ltk
          bond-authenticated
          bond-secure-connections
          *bonds*
          *bond-file*
          save-bonds
          load-bonds
          store-bond
          forget-bond
          find-bond
          bond-from-session
          ;; Giving the adapter back (src/teardown.lisp)
          *open-att-channels*
          register-att-channel
          unregister-att-channel
          close-all-att-channels
          install-adapter-teardown))
