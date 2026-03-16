```cobol
       POPULATE-AUTH-DETAILS.


           IF ERR-FLG-OFF
               MOVE PA-CARD-NUM               TO CARDNUMO

               MOVE PA-AUTH-ORIG-DATE(1:2)    TO WS-CURDATE-YY
               MOVE PA-AUTH-ORIG-DATE(3:2)    TO WS-CURDATE-MM
               MOVE PA-AUTH-ORIG-DATE(5:2)    TO WS-CURDATE-DD
               MOVE WS-CURDATE-MM-DD-YY       TO WS-AUTH-DATE
               MOVE WS-AUTH-DATE              TO AUTHDTO

               MOVE PA-AUTH-ORIG-TIME(1:2)    TO WS-AUTH-TIME(1:2)
               MOVE PA-AUTH-ORIG-TIME(3:2)    TO WS-AUTH-TIME(4:2)
               MOVE PA-AUTH-ORIG-TIME(5:2)    TO WS-AUTH-TIME(7:2)
               MOVE WS-AUTH-TIME              TO AUTHTMO

               MOVE PA-APPROVED-AMT           TO WS-AUTH-AMT
               MOVE WS-AUTH-AMT               TO AUTHAMTO

               IF PA-AUTH-RESP-CODE = '00'
                  MOVE 'A'                    TO AUTHRSPO
                  MOVE DFHGREEN               TO AUTHRSPC
               ELSE
                  MOVE 'D'                    TO AUTHRSPO
                  MOVE DFHRED                 TO AUTHRSPC
               END-IF

               SEARCH ALL WS-DECLINE-REASON-TAB
                   AT END
                        MOVE '9999'                     TO AUTHRSNO
                        MOVE '-'                        TO AUTHRSNO(5:1)
                        MOVE 'ERROR'                    TO AUTHRSNO(6:)
                   WHEN DECL-CODE(WS-DECL-RSN-IDX) = PA-AUTH-RESP-REASON
                        MOVE PA-AUTH-RESP-REASON        TO AUTHRSNO
                        MOVE '-'                        TO AUTHRSNO(5:1)
                        MOVE DECL-DESC(WS-DECL-RSN-IDX) TO AUTHRSNO(6:)
               END-SEARCH


               MOVE PA-PROCESSING-CODE        TO AUTHCDO
               MOVE PA-POS-ENTRY-MODE         TO POSEMDO
               MOVE PA-MESSAGE-SOURCE         TO AUTHSRCO
               MOVE PA-MERCHANT-CATAGORY-CODE TO MCCCDO

               MOVE PA-CARD-EXPIRY-DATE(1:2)  TO CRDEXPO(1:2)
               MOVE '/'                       TO CRDEXPO(3:1)
               MOVE PA-CARD-EXPIRY-DATE(3:2)  TO CRDEXPO(4:2)

               MOVE PA-AUTH-TYPE              TO AUTHTYPO
               MOVE PA-TRANSACTION-ID         TO TRNIDO
               MOVE PA-MATCH-STATUS           TO AUTHMTCO

               IF PA-FRAUD-CONFIRMED OR PA-FRAUD-REMOVED
                  MOVE PA-AUTH-FRAUD          TO AUTHFRDO(1:1)
                  MOVE '-'                    TO AUTHFRDO(2:1)
                  MOVE PA-FRAUD-RPT-DATE      TO AUTHFRDO(3:)
               ELSE
                  MOVE '-'                    TO AUTHFRDO
               END-IF

               MOVE PA-MERCHANT-NAME          TO MERNAMEO
               MOVE PA-MERCHANT-ID            TO MERIDO
               MOVE PA-MERCHANT-CITY          TO MERCITYO
               MOVE PA-MERCHANT-STATE         TO MERSTO
               MOVE PA-MERCHANT-ZIP           TO MERZIPO
           END-IF
           .
```
