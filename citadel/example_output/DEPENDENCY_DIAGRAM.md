# Dependency Graph Summary

**Generated:** 2026-01-26T14:05:14.994638
**Source Root:** `/Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq`
**Version:** 1.0

---

## Statistics

- **Files Analyzed:** 28
- **Files Skipped:** 12
- **Files Failed:** 0
- **Total Artifacts:** 153
- **Total Relationships:** 147
- **Unresolved References:** 72
- **Resolution Rate:** 6712.3%

### Artifacts by Type

| Type | Count |
|------|-------|
| column | 3 |
| copybook | 11 |
| index | 1 |
| map | 2 |
| paragraph | 113 |
| procedure | 5 |
| program | 8 |
| record_layout | 7 |
| screen | 2 |
| table | 1 |

### Relationships by Type

| Type | Count |
|------|-------|
| includes | 28 |
| performs | 119 |

### Languages Detected

- **CICS BMS** (spec: `bms`)
- **COBOL** (spec: `cobol`)
- **COBOL Copybook** (spec: `copybook`)
- **DB2 DDL** (spec: `db2_ddl`)
- **JCL** (spec: `jcl`)

---

## Dependency Diagram

```mermaid
flowchart LR
    subgraph code
        n_procedure_LOADPADB[LOADPADB]
        n_paragraph_STEP01[STEP01]
        n_procedure_DBPAUTP0[DBPAUTP0]
        n_paragraph_STEPDEL[STEPDEL]
        n_paragraph_UNLOAD[UNLOAD]
        n_procedure_UNLDGSAM[UNLDGSAM]
        n_procedure_CBPAUP0J[CBPAUP0J]
        n_procedure_UNLDPADB[UNLDPADB]
        n_paragraph_STEP0[STEP0]
        n_copybook_COPAU01[/COPAU01/]
        n_copybook_COPAU00[/COPAU00/]
        n_copybook_CCPAURLY[/CCPAURLY/]
        n_copybook_CIPAUSMY[/CIPAUSMY/]
        n_copybook_PASFLPCB[/PASFLPCB/]
        n_copybook_IMSFUNCS[/IMSFUNCS/]
        n_copybook_PAUTBPCB[/PAUTBPCB/]
        n_copybook_CIPAUDTY[/CIPAUDTY/]
        n_copybook_CCPAURQY[/CCPAURQY/]
        n_copybook_CCPAUERY[/CCPAUERY/]
        n_copybook_PADFLPCB[/PADFLPCB/]
        n_program_PAUDBLOD[PAUDBLOD]
        n_paragraph_PAUTBPCB[PAUTBPCB]
        n_paragraph_MAIN_PARA[MAIN-PARA]
        n_paragraph_GOBACK[GOBACK]
        n_paragraph_1000_INITIALIZE[1000-INITIALIZE]
        n_paragraph_01810026[01810026]
        n_paragraph_END_IF[END-IF]
        n_paragraph_1000_EXIT[1000-EXIT]
        n_paragraph_EXIT[EXIT]
        n_paragraph_2000_READ_ROOT_SEG_FILE[2000-READ-ROOT-SEG-FILE]
        n_paragraph_2000_EXIT[2000-EXIT]
        n_paragraph_2100_INSERT_ROOT_SEG[2100-INSERT-ROOT-SEG]
        n_paragraph_ROOT_UNQUAL_SSA[ROOT-UNQUAL-SSA]
        n_paragraph_2100_EXIT[2100-EXIT]
        n_paragraph_3000_READ_CHILD_SEG_FILE[3000-READ-CHILD-SEG-FILE]
        n_paragraph_3000_EXIT[3000-EXIT]
        n_paragraph_3100_INSERT_CHILD_SEG[3100-INSERT-CHILD-SEG]
        n_paragraph_ROOT_QUAL_SSA[ROOT-QUAL-SSA]
        n_paragraph_3100_EXIT[3100-EXIT]
        n_paragraph_3200_INSERT_IMS_CALL[3200-INSERT-IMS-CALL]
        n_paragraph_CHILD_UNQUAL_SSA[CHILD-UNQUAL-SSA]
        n_paragraph_3200_EXIT[3200-EXIT]
        n_paragraph_4000_FILE_CLOSE[4000-FILE-CLOSE]
        n_paragraph_4000_EXIT[4000-EXIT]
        n_paragraph_9999_ABEND[9999-ABEND]
        n_paragraph_9999_EXIT[9999-EXIT]
        n_program_COPAUS2C[COPAUS2C]
        n_paragraph_END_EXEC[END-EXEC]
        n_paragraph_FRAUD_UPDATE[FRAUD-UPDATE]
        n_program_COPAUA0C[COPAUA0C]
        n_paragraph_1100_OPEN_REQUEST_QUEUE[1100-OPEN-REQUEST-QUEUE]
        n_paragraph_1100_EXIT[1100-EXIT]
        n_paragraph_1200_SCHEDULE_PSB[1200-SCHEDULE-PSB]
        n_paragraph_1200_EXIT[1200-EXIT]
        n_paragraph_2000_MAIN_PROCESS[2000-MAIN-PROCESS]
        n_paragraph_END_PERFORM[END-PERFORM]
        n_paragraph_2100_EXTRACT_REQUEST_MSG[2100-EXTRACT-REQUEST-MSG]
        n_paragraph_3100_READ_REQUEST_MQ[3100-READ-REQUEST-MQ]
        n_paragraph_5000_PROCESS_AUTH[5000-PROCESS-AUTH]
        n_paragraph_5000_EXIT[5000-EXIT]
        n_paragraph_5100_READ_XREF_RECORD[5100-READ-XREF-RECORD]
        n_paragraph_END_EVALUATE[END-EVALUATE]
        n_paragraph_5100_EXIT[5100-EXIT]
        n_paragraph_5200_READ_ACCT_RECORD[5200-READ-ACCT-RECORD]
        n_paragraph_5200_EXIT[5200-EXIT]
        n_paragraph_5300_READ_CUST_RECORD[5300-READ-CUST-RECORD]
        n_paragraph_5300_EXIT[5300-EXIT]
        n_paragraph_5500_READ_AUTH_SUMMRY[5500-READ-AUTH-SUMMRY]
        n_paragraph_5500_EXIT[5500-EXIT]
        n_paragraph_5600_READ_PROFILE_DATA[5600-READ-PROFILE-DATA]
        n_paragraph_CONTINUE[CONTINUE]
        n_paragraph_5600_EXIT[5600-EXIT]
        n_paragraph_6000_MAKE_DECISION[6000-MAKE-DECISION]
        n_paragraph_END_STRING[END-STRING]
        n_paragraph_6000_EXIT[6000-EXIT]
        n_paragraph_7100_SEND_RESPONSE[7100-SEND-RESPONSE]
        n_paragraph_7100_EXIT[7100-EXIT]
        n_paragraph_8000_WRITE_AUTH_TO_DB[8000-WRITE-AUTH-TO-DB]
        n_paragraph_8000_EXIT[8000-EXIT]
        n_paragraph_8400_UPDATE_SUMMARY[8400-UPDATE-SUMMARY]
        n_paragraph_8400_EXIT[8400-EXIT]
        n_paragraph_8500_INSERT_AUTH[8500-INSERT-AUTH]
        n_paragraph_8500_EXIT[8500-EXIT]
        n_paragraph_9000_TERMINATE[9000-TERMINATE]
        n_paragraph_9000_EXIT[9000-EXIT]
        n_paragraph_9100_CLOSE_REQUEST_QUEUE[9100-CLOSE-REQUEST-QUEUE]
        n_paragraph_9100_EXIT[9100-EXIT]
        n_paragraph_9500_LOG_ERROR[9500-LOG-ERROR]
        n_paragraph_9500_EXIT[9500-EXIT]
        n_paragraph_9990_END_ROUTINE[9990-END-ROUTINE]
        n_paragraph_9990_EXIT[9990-EXIT]
        n_program_DBUNLDGS[DBUNLDGS]
        n_paragraph_PADFLPCB[PADFLPCB]
        n_paragraph_01680000[01680000]
        n_paragraph_2000_FIND_NEXT_AUTH_SUMMARY[2000-FIND-NEXT-AUTH-SUMMARY]
        n_paragraph_3000_FIND_NEXT_AUTH_DTL[3000-FIND-NEXT-AUTH-DTL]
        n_paragraph_3100_INSERT_PARENT_SEG_GSAM[3100-INSERT-PARENT-SEG-GSAM]
        n_paragraph_PENDING_AUTH_SUMMARY[PENDING-AUTH-SUMMARY]
        n_paragraph_3200_INSERT_CHILD_SEG_GSAM[3200-INSERT-CHILD-SEG-GSAM]
        n_paragraph_PENDING_AUTH_DETAILS[PENDING-AUTH-DETAILS]
        n_program_CBPAUP0C[CBPAUP0C]
        n_paragraph_PGM_PCB_MASK[PGM-PCB-MASK]
        n_paragraph_4000_CHECK_IF_EXPIRED[4000-CHECK-IF-EXPIRED]
        n_paragraph_5000_DELETE_AUTH_DTL[5000-DELETE-AUTH-DTL]
        n_paragraph_6000_DELETE_AUTH_SUMMARY[6000-DELETE-AUTH-SUMMARY]
        n_paragraph_9000_TAKE_CHECKPOINT[9000-TAKE-CHECKPOINT]
        n_program_PAUDBUNL[PAUDBUNL]
        n_program_COPAUS1C[COPAUS1C]
        n_paragraph_PROCESS_ENTER_KEY[PROCESS-ENTER-KEY]
        n_paragraph_MARK_AUTH_FRAUD[MARK-AUTH-FRAUD]
        n_paragraph_PROCESS_PF8_KEY[PROCESS-PF8-KEY]
        n_paragraph_POPULATE_AUTH_DETAILS[POPULATE-AUTH-DETAILS]
        n_paragraph_RETURN_TO_PREV_SCREEN[RETURN-TO-PREV-SCREEN]
        n_paragraph_SEND_AUTHVIEW_SCREEN[SEND-AUTHVIEW-SCREEN]
        n_paragraph_RECEIVE_AUTHVIEW_SCREEN[RECEIVE-AUTHVIEW-SCREEN]
        n_paragraph_POPULATE_HEADER_INFO[POPULATE-HEADER-INFO]
        n_paragraph_READ_AUTH_RECORD[READ-AUTH-RECORD]
        n_paragraph_READ_NEXT_AUTH_RECORD[READ-NEXT-AUTH-RECORD]
        n_paragraph_UPDATE_AUTH_DETAILS[UPDATE-AUTH-DETAILS]
        n_paragraph_TAKE_SYNCPOINT[TAKE-SYNCPOINT]
        n_paragraph_ROLL_BACK[ROLL-BACK]
        n_paragraph_SCHEDULE_PSB[SCHEDULE-PSB]
        n_program_COPAUS0C[COPAUS0C]
        n_paragraph_GATHER_DETAILS[GATHER-DETAILS]
        n_paragraph_PROCESS_PF7_KEY[PROCESS-PF7-KEY]
        n_paragraph_PROCESS_PAGE_FORWARD[PROCESS-PAGE-FORWARD]
        n_paragraph_GET_AUTHORIZATIONS[GET-AUTHORIZATIONS]
        n_paragraph_REPOSITION_AUTHORIZATIONS[REPOSITION-AUTHORIZATIONS]
        n_paragraph_POPULATE_AUTH_LIST[POPULATE-AUTH-LIST]
        n_paragraph_INITIALIZE_AUTH_DATA[INITIALIZE-AUTH-DATA]
        n_paragraph_SEND_PAULST_SCREEN[SEND-PAULST-SCREEN]
        n_paragraph_RECEIVE_PAULST_SCREEN[RECEIVE-PAULST-SCREEN]
        n_paragraph_GATHER_ACCOUNT_DETAILS[GATHER-ACCOUNT-DETAILS]
        n_paragraph_GETCARDXREF_BYACCT[GETCARDXREF-BYACCT]
        n_paragraph_GETACCTDATA_BYACCT[GETACCTDATA-BYACCT]
        n_paragraph_GETCUSTDATA_BYCUST[GETCUSTDATA-BYCUST]
        n_paragraph_GET_AUTH_SUMMARY[GET-AUTH-SUMMARY]
    end
    subgraph data
        n_record_layout_COPAU1AI[(COPAU1AI)]
        n_record_layout_COPAU0AI[(COPAU0AI)]
        n_column_05:PA_RL_CARD_NUM(05:PA-RL-CARD-NUM)
        n_column_05:PA_ACCT_ID(05:PA-ACCT-ID)
        n_record_layout_PASFLPCB[(PASFLPCB)]
        n_record_layout_FUNC_CODES[(FUNC-CODES)]
        n_record_layout_PAUTBPCB[(PAUTBPCB)]
        n_column_05:PA_RQ_AUTH_DATE(05:PA-RQ-AUTH-DATE)
        n_record_layout_ERROR_LOG_RECORD[(ERROR-LOG-RECORD)]
        n_record_layout_PADFLPCB[(PADFLPCB)]
        n_index_XAUTHFRD(XAUTHFRD)
        n_table_AUTHFRDS[(AUTHFRDS)]
    end
    subgraph interface
        n_map_COPAU00{{COPAU00}}
        n_screen_COPAU0A{{COPAU0A}}
        n_map_COPAU01{{COPAU01}}
        n_screen_COPAU1A{{COPAU1A}}
    end

    n_program_PAUDBLOD -->|includes| n_copybook_IMSFUNCS
    n_program_PAUDBLOD -->|includes| n_copybook_CIPAUSMY
    n_program_PAUDBLOD -->|includes| n_copybook_CIPAUDTY
    n_program_PAUDBLOD -->|includes| n_copybook_PAUTBPCB
    n_program_COPAUS2C -->|includes| n_copybook_CIPAUDTY
    n_program_COPAUA0C -->|includes| n_copybook_CCPAURQY
    n_program_COPAUA0C -->|includes| n_copybook_CCPAURLY
    n_program_COPAUA0C -->|includes| n_copybook_CCPAUERY
    n_program_COPAUA0C -->|includes| n_copybook_CIPAUSMY
    n_program_COPAUA0C -->|includes| n_copybook_CIPAUDTY
    n_program_DBUNLDGS -->|includes| n_copybook_IMSFUNCS
    n_program_DBUNLDGS -->|includes| n_copybook_CIPAUSMY
    n_program_DBUNLDGS -->|includes| n_copybook_CIPAUDTY
    n_program_DBUNLDGS -->|includes| n_copybook_PAUTBPCB
    n_program_DBUNLDGS -->|includes| n_copybook_PASFLPCB
    n_program_DBUNLDGS -->|includes| n_copybook_PADFLPCB
    n_program_CBPAUP0C -->|includes| n_copybook_CIPAUSMY
    n_program_CBPAUP0C -->|includes| n_copybook_CIPAUDTY
    n_program_PAUDBUNL -->|includes| n_copybook_IMSFUNCS
    n_program_PAUDBUNL -->|includes| n_copybook_CIPAUSMY
    n_program_PAUDBUNL -->|includes| n_copybook_CIPAUDTY
    n_program_PAUDBUNL -->|includes| n_copybook_PAUTBPCB
    n_program_COPAUS1C -->|includes| n_copybook_COPAU01
    n_program_COPAUS1C -->|includes| n_copybook_CIPAUSMY
    n_program_COPAUS1C -->|includes| n_copybook_CIPAUDTY
    n_program_COPAUS0C -->|includes| n_copybook_COPAU00
    n_program_COPAUS0C -->|includes| n_copybook_CIPAUSMY
    n_program_COPAUS0C -->|includes| n_copybook_CIPAUDTY
```

---

## Artifacts

| ID | Name | Type | Category | Language | Defined In |
|----|------|------|----------|----------|------------|
| `column::05:PA-ACCT-ID` | 05:PA-ACCT-ID | column | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy:1-31 |
| `column::05:PA-RL-CARD-NUM` | 05:PA-RL-CARD-NUM | column | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAURLY.cpy:1-24 |
| `column::05:PA-RQ-AUTH-DATE` | 05:PA-RQ-AUTH-DATE | column | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAURQY.cpy:1-36 |
| `copybook::CCPAUERY` | CCPAUERY | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAUERY.cpy:1 |
| `copybook::CCPAURLY` | CCPAURLY | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAURLY.cpy:1 |
| `copybook::CCPAURQY` | CCPAURQY | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAURQY.cpy:1 |
| `copybook::CIPAUDTY` | CIPAUDTY | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CIPAUDTY.cpy:1-54 |
| `copybook::CIPAUSMY` | CIPAUSMY | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy:1 |
| `copybook::COPAU00` | COPAU00 | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy-bms/COPAU00.cpy:1 |
| `copybook::COPAU01` | COPAU01 | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy-bms/COPAU01.cpy:1 |
| `copybook::IMSFUNCS` | IMSFUNCS | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/IMSFUNCS.cpy:1 |
| `copybook::PADFLPCB` | PADFLPCB | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PADFLPCB.CPY:1 |
| `copybook::PASFLPCB` | PASFLPCB | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PASFLPCB.CPY:1 |
| `copybook::PAUTBPCB` | PAUTBPCB | copybook | code | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PAUTBPCB.CPY:1 |
| `index::XAUTHFRD` | XAUTHFRD | index | data | DB2 DDL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/ddl/XAUTHFRD.ddl:1-4 |
| `map::COPAU00` | COPAU00 | map | interface | CICS BMS | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/bms/COPAU00.bms:19-25 |
| `map::COPAU01` | COPAU01 | map | interface | CICS BMS | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/bms/COPAU01.bms:19-25 |
| `paragraph::01680000` | 01680000 | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:193-194 |
| `paragraph::01810026` | 01810026 | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:184-199 |
| `paragraph::1000-EXIT` | 1000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:201-203 |
| `paragraph::1000-INITIALIZE` | 1000-INITIALIZE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:171-183 |
| `paragraph::1100-EXIT` | 1100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:285-286 |
| `paragraph::1100-OPEN-REQUEST-QUEUE` | 1100-OPEN-REQUEST-QUEUE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:251-282 |
| `paragraph::1200-EXIT` | 1200-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:319 |
| `paragraph::1200-SCHEDULE-PSB` | 1200-SCHEDULE-PSB | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:288-316 |
| `paragraph::2000-EXIT` | 2000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:248 |
| `paragraph::2000-FIND-NEXT-AUTH-SUMMARY` | 2000-FIND-NEXT-AUTH-SUMMARY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:205-215 |
| `paragraph::2000-MAIN-PROCESS` | 2000-MAIN-PROCESS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:322-343 |
| `paragraph::2000-READ-ROOT-SEG-FILE` | 2000-READ-ROOT-SEG-FILE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:220-236 |
| `paragraph::2100-EXIT` | 2100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:381-382 |
| `paragraph::2100-EXTRACT-REQUEST-MSG` | 2100-EXTRACT-REQUEST-MSG | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:349-380 |
| `paragraph::2100-INSERT-ROOT-SEG` | 2100-INSERT-ROOT-SEG | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:242-246 |
| `paragraph::3000-EXIT` | 3000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:285 |
| `paragraph::3000-FIND-NEXT-AUTH-DTL` | 3000-FIND-NEXT-AUTH-DTL | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:250-259 |
| `paragraph::3000-READ-CHILD-SEG-FILE` | 3000-READ-CHILD-SEG-FILE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:266-288 |
| `paragraph::3100-EXIT` | 3100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:316 |
| `paragraph::3100-INSERT-CHILD-SEG` | 3100-INSERT-CHILD-SEG | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:292-298 |
| `paragraph::3100-INSERT-PARENT-SEG-GSAM` | 3100-INSERT-PARENT-SEG-GSAM | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:298-303 |
| `paragraph::3100-READ-REQUEST-MQ` | 3100-READ-REQUEST-MQ | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:384-430 |
| `paragraph::3200-EXIT` | 3200-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:335 |
| `paragraph::3200-INSERT-CHILD-SEG-GSAM` | 3200-INSERT-CHILD-SEG-GSAM | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:318-322 |
| `paragraph::3200-INSERT-IMS-CALL` | 3200-INSERT-IMS-CALL | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:317-323 |
| `paragraph::4000-CHECK-IF-EXPIRED` | 4000-CHECK-IF-EXPIRED | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:275-276 |
| `paragraph::4000-EXIT` | 4000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:305 |
| `paragraph::4000-FILE-CLOSE` | 4000-FILE-CLOSE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:287-296 |
| `paragraph::5000-DELETE-AUTH-DTL` | 5000-DELETE-AUTH-DTL | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:301-302 |
| `paragraph::5000-EXIT` | 5000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:324 |
| `paragraph::5000-PROCESS-AUTH` | 5000-PROCESS-AUTH | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:436-464 |
| `paragraph::5100-EXIT` | 5100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:515-516 |
| `paragraph::5100-READ-XREF-RECORD` | 5100-READ-XREF-RECORD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:470-512 |
| `paragraph::5200-EXIT` | 5200-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:563-564 |
| `paragraph::5200-READ-ACCT-RECORD` | 5200-READ-ACCT-RECORD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:518-560 |
| `paragraph::5300-EXIT` | 5300-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:611-612 |
| `paragraph::5300-READ-CUST-RECORD` | 5300-READ-CUST-RECORD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:566-608 |
| `paragraph::5500-EXIT` | 5500-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:642-643 |
| `paragraph::5500-READ-AUTH-SUMMRY` | 5500-READ-AUTH-SUMMRY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:614-639 |
| `paragraph::5600-EXIT` | 5600-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:652-653 |
| `paragraph::5600-READ-PROFILE-DATA` | 5600-READ-PROFILE-DATA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:645-647 |
| `paragraph::6000-DELETE-AUTH-SUMMARY` | 6000-DELETE-AUTH-SUMMARY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:326-327 |
| `paragraph::6000-EXIT` | 6000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:348 |
| `paragraph::6000-MAKE-DECISION` | 6000-MAKE-DECISION | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:655-730 |
| `paragraph::7100-EXIT` | 7100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:781-782 |
| `paragraph::7100-SEND-RESPONSE` | 7100-SEND-RESPONSE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:736-778 |
| `paragraph::8000-EXIT` | 8000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:793-794 |
| `paragraph::8000-WRITE-AUTH-TO-DB` | 8000-WRITE-AUTH-TO-DB | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:784-792 |
| `paragraph::8400-EXIT` | 8400-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:849-850 |
| `paragraph::8400-UPDATE-SUMMARY` | 8400-UPDATE-SUMMARY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:796-846 |
| `paragraph::8500-EXIT` | 8500-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:934-935 |
| `paragraph::8500-INSERT-AUTH` | 8500-INSERT-AUTH | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:852-931 |
| `paragraph::9000-EXIT` | 9000-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:373 |
| `paragraph::9000-TAKE-CHECKPOINT` | 9000-TAKE-CHECKPOINT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:350-351 |
| `paragraph::9000-TERMINATE` | 9000-TERMINATE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:937-948 |
| `paragraph::9100-CLOSE-REQUEST-QUEUE` | 9100-CLOSE-REQUEST-QUEUE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:952-976 |
| `paragraph::9100-EXIT` | 9100-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:978-979 |
| `paragraph::9500-EXIT` | 9500-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:1012 |
| `paragraph::9500-LOG-ERROR` | 9500-LOG-ERROR | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:981-1009 |
| `paragraph::9990-END-ROUTINE` | 9990-END-ROUTINE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:1014-1021 |
| `paragraph::9990-EXIT` | 9990-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:1024 |
| `paragraph::9999-ABEND` | 9999-ABEND | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:307-313 |
| `paragraph::9999-EXIT` | 9999-EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:315-316 |
| `paragraph::CHILD-UNQUAL-SSA` | CHILD-UNQUAL-SSA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:260-282 |
| `paragraph::CONTINUE` | CONTINUE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:648-651 |
| `paragraph::END-EVALUATE` | END-EVALUATE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:996-997 |
| `paragraph::END-EXEC` | END-EXEC | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:721-722 |
| `paragraph::END-IF` | END-IF | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:1030-1031 |
| `paragraph::END-PERFORM` | END-PERFORM | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:661-662 |
| `paragraph::END-STRING` | END-STRING | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:731-732 |
| `paragraph::EXIT` | EXIT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:317 |
| `paragraph::FRAUD-UPDATE` | FRAUD-UPDATE | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:221-242 |
| `paragraph::GATHER-ACCOUNT-DETAILS` | GATHER-ACCOUNT-DETAILS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:748-749 |
| `paragraph::GATHER-DETAILS` | GATHER-DETAILS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:339-341 |
| `paragraph::GET-AUTH-SUMMARY` | GET-AUTH-SUMMARY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:964-965 |
| `paragraph::GET-AUTHORIZATIONS` | GET-AUTHORIZATIONS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:455-457 |
| `paragraph::GETACCTDATA-BYACCT` | GETACCTDATA-BYACCT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:863-864 |
| `paragraph::GETCARDXREF-BYACCT` | GETCARDXREF-BYACCT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:809-811 |
| `paragraph::GETCUSTDATA-BYCUST` | GETCUSTDATA-BYCUST | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:913-914 |
| `paragraph::GOBACK` | GOBACK | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:314 |
| `paragraph::INITIALIZE-AUTH-DATA` | INITIALIZE-AUTH-DATA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:606-607 |
| `paragraph::MAIN-PARA` | MAIN-PARA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:177 |
| `paragraph::MARK-AUTH-FRAUD` | MARK-AUTH-FRAUD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:229 |
| `paragraph::PADFLPCB` | PADFLPCB | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:167-175 |
| `paragraph::PAUTBPCB` | PAUTBPCB | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:165 |
| `paragraph::PENDING-AUTH-DETAILS` | PENDING-AUTH-DETAILS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:323-333 |
| `paragraph::PENDING-AUTH-SUMMARY` | PENDING-AUTH-SUMMARY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:304-314 |
| `paragraph::PGM-PCB-MASK` | PGM-PCB-MASK | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:133 |
| `paragraph::POPULATE-AUTH-DETAILS` | POPULATE-AUTH-DETAILS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:290 |
| `paragraph::POPULATE-AUTH-LIST` | POPULATE-AUTH-LIST | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:520-521 |
| `paragraph::POPULATE-HEADER-INFO` | POPULATE-HEADER-INFO | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:723-725 |
| `paragraph::PROCESS-ENTER-KEY` | PROCESS-ENTER-KEY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:258-260 |
| `paragraph::PROCESS-PAGE-FORWARD` | PROCESS-PAGE-FORWARD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:413-414 |
| `paragraph::PROCESS-PF7-KEY` | PROCESS-PF7-KEY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:359-361 |
| `paragraph::PROCESS-PF8-KEY` | PROCESS-PF8-KEY | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:386-387 |
| `paragraph::READ-AUTH-RECORD` | READ-AUTH-RECORD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:430 |
| `paragraph::READ-NEXT-AUTH-RECORD` | READ-NEXT-AUTH-RECORD | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:492 |
| `paragraph::RECEIVE-AUTHVIEW-SCREEN` | RECEIVE-AUTHVIEW-SCREEN | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:397 |
| `paragraph::RECEIVE-PAULST-SCREEN` | RECEIVE-PAULST-SCREEN | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:710-711 |
| `paragraph::REPOSITION-AUTHORIZATIONS` | REPOSITION-AUTHORIZATIONS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:487 |
| `paragraph::RETURN-TO-PREV-SCREEN` | RETURN-TO-PREV-SCREEN | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:663-664 |
| `paragraph::ROLL-BACK` | ROLL-BACK | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:561-564 |
| `paragraph::ROOT-QUAL-SSA` | ROOT-QUAL-SSA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:299-313 |
| `paragraph::ROOT-UNQUAL-SSA` | ROOT-UNQUAL-SSA | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:216-247 |
| `paragraph::SCHEDULE-PSB` | SCHEDULE-PSB | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:998-1000 |
| `paragraph::SEND-AUTHVIEW-SCREEN` | SEND-AUTHVIEW-SCREEN | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:371-372 |
| `paragraph::SEND-PAULST-SCREEN` | SEND-PAULST-SCREEN | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:678-680 |
| `paragraph::STEP0` | STEP0 | paragraph | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL:25-37 |
| `paragraph::STEP01` | STEP01 | paragraph | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL:38-69 |
| `paragraph::STEPDEL` | STEPDEL | paragraph | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/DBPAUTP0.jcl:7-14 |
| `paragraph::TAKE-SYNCPOINT` | TAKE-SYNCPOINT | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:553-556 |
| `paragraph::UNLOAD` | UNLOAD | paragraph | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/DBPAUTP0.jcl:15-47 |
| `paragraph::UPDATE-AUTH-DETAILS` | UPDATE-AUTH-DETAILS | paragraph | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:519 |
| `procedure::CBPAUP0J` | CBPAUP0J | procedure | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl:1-23 |
| `procedure::DBPAUTP0` | DBPAUTP0 | procedure | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/DBPAUTP0.jcl:1-6 |
| `procedure::LOADPADB` | LOADPADB | procedure | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/LOADPADB.JCL:1-25 |
| `procedure::UNLDGSAM` | UNLDGSAM | procedure | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDGSAM.JCL:1-25 |
| `procedure::UNLDPADB` | UNLDPADB | procedure | code | JCL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL:1-24 |
| `program::CBPAUP0C` | CBPAUP0C | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:23-25 |
| `program::COPAUA0C` | COPAUA0C | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:23-25 |
| `program::COPAUS0C` | COPAUS0C | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:23-25 |
| `program::COPAUS1C` | COPAUS1C | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:23-25 |
| `program::COPAUS2C` | COPAUS2C | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:23-25 |
| `program::DBUNLDGS` | DBUNLDGS | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:18-20 |
| `program::PAUDBLOD` | PAUDBLOD | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:18-20 |
| `program::PAUDBUNL` | PAUDBUNL | program | code | COBOL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:18-20 |
| `record_layout::COPAU0AI` | COPAU0AI | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy-bms/COPAU00.cpy:1-764 |
| `record_layout::COPAU1AI` | COPAU1AI | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy-bms/COPAU01.cpy:1-344 |
| `record_layout::ERROR-LOG-RECORD` | ERROR-LOG-RECORD | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/CCPAUERY.cpy:1-40 |
| `record_layout::FUNC-CODES` | FUNC-CODES | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/IMSFUNCS.cpy:1-27 |
| `record_layout::PADFLPCB` | PADFLPCB | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PADFLPCB.CPY:1-26 |
| `record_layout::PASFLPCB` | PASFLPCB | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PASFLPCB.CPY:1-26 |
| `record_layout::PAUTBPCB` | PAUTBPCB | record_layout | data | COBOL Copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cpy/PAUTBPCB.CPY:1-26 |
| `screen::COPAU0A` | COPAU0A | screen | interface | CICS BMS | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/bms/COPAU00.bms:26-515 |
| `screen::COPAU1A` | COPAU1A | screen | interface | CICS BMS | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/bms/COPAU01.bms:26-294 |
| `table::AUTHFRDS` | AUTHFRDS | table | data | DB2 DDL | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl:1-28 |

---

## Unresolved References

| Reference | Expected Type | Location | Reason |
|-----------|---------------|----------|--------|
| `DFSRRC00` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/LOADPADB.JCL:26 | no match found for 'DFSRRC00' |
| `DFSRRC00` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/DBPAUTP0.jcl:15 | no match found for 'DFSRRC00' |
| `DFSRRC00` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDGSAM.JCL:26 | no match found for 'DFSRRC00' |
| `DFSRRC00` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl:24 | no match found for 'DFSRRC00' |
| `DFSRRC00` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL:38 | no match found for 'DFSRRC00' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:244 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:296 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:321 | no match found for 'CBLTDLI' |
| `THRU` | N/A | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL:309 | no match found for 'THRU' |
| `CARDDEMO` | table | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:141-142 | no match found for 'CARDDEMO' |
| `CARDDEMO` | table | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:222-223 | no match found for 'CARDDEMO' |
| `MQOPEN` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:262 | no match found for 'MQOPEN' |
| `MQGET` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:400 | no match found for 'MQGET' |
| `MQPUT1` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:758 | no match found for 'MQPUT1' |
| `MQCLOSE` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:956 | no match found for 'MQCLOSE' |
| `IF` | N/A | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:268-270 | no match found for 'IF' |
| `IF` | N/A | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:409-410 | no match found for 'IF' |
| `IF` | N/A | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:766-767 | no match found for 'IF' |
| `IF` | N/A | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:961-963 | no match found for 'IF' |
| `UNTIL` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:326 | no match found for 'UNTIL' |
| `WS-CCXREF-FILE` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:477-478 | no match found for 'WS-CCXREF-FILE' |
| `WS-ACCTFILENAME` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:525-526 | no match found for 'WS-ACCTFILENAME' |
| `WS-CUSTFILENAME` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:573-574 | no match found for 'WS-CUSTFILENAME' |
| `CMQODV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:149 | no match found for 'CMQODV' |
| `CMQMDV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:152 | no match found for 'CMQMDV' |
| `CMQODV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:155 | no match found for 'CMQODV' |
| `CMQMDV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:158 | no match found for 'CMQMDV' |
| `CMQV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:161 | no match found for 'CMQV' |
| `CMQTML` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:164 | no match found for 'CMQTML' |
| `CMQPMOV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:167 | no match found for 'CMQPMOV' |
| `CMQGMOV` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:170 | no match found for 'CMQGMOV' |
| `CVACT03Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:203 | no match found for 'CVACT03Y' |
| `CVACT01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:206 | no match found for 'CVACT01Y' |
| `CVCUS01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:209 | no match found for 'CVCUS01Y' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:222 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:267 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:302 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL:321 | no match found for 'CBLTDLI' |
| `UNTIL` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:142 | no match found for 'UNTIL' |
| `UNTIL` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:146 | no match found for 'UNTIL' |
| `IF` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:154-156 | no match found for 'IF' |
| `PERFORM` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:167-169 | no match found for 'PERFORM' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:213 | no match found for 'CBLTDLI' |
| `CBLTDLI` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL:257 | no match found for 'CBLTDLI' |
| `WS-PGM-AUTH-FRAUD` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:248-249 | no match found for 'WS-PGM-AUTH-FRAUD' |
| `CDEMO-TO-PROGRAM` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:367-368 | no match found for 'CDEMO-TO-PROGRAM' |
| `COCOM01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:109 | no match found for 'COCOM01Y' |
| `COTTL01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:126 | no match found for 'COTTL01Y' |
| `CSDAT01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:129 | no match found for 'CSDAT01Y' |
| `CSMSG01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:132 | no match found for 'CSMSG01Y' |
| `CSMSG02Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:135 | no match found for 'CSMSG02Y' |
| `DFHAID` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:148 | no match found for 'DFHAID' |
| `DFHBMSCA` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:149 | no match found for 'DFHBMSCA' |
| `UNTIL` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:424 | no match found for 'UNTIL' |
| `IF` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:443-445 | no match found for 'IF' |
| `VARYING` | paragraph | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:611 | no match found for 'VARYING' |
| `CDEMO-TO-PROGRAM` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:322-323 | no match found for 'CDEMO-TO-PROGRAM' |
| `CDEMO-TO-PROGRAM` | program | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:674-675 | no match found for 'CDEMO-TO-PROGRAM' |
| `WS-CARDXREFNAME-ACCT-PATH` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:818-819 | no match found for 'WS-CARDXREFNAME-ACCT-PATH' |
| `WS-ACCTFILENAME` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:869-870 | no match found for 'WS-ACCTFILENAME' |
| `WS-CUSTFILENAME` | file | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:920-921 | no match found for 'WS-CUSTFILENAME' |
| `COCOM01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:116 | no match found for 'COCOM01Y' |
| `COTTL01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:132 | no match found for 'COTTL01Y' |
| `CSDAT01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:135 | no match found for 'CSDAT01Y' |
| `CSMSG01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:138 | no match found for 'CSMSG01Y' |
| `CSMSG02Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:141 | no match found for 'CSMSG02Y' |
| `CVACT01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:144 | no match found for 'CVACT01Y' |
| `CVACT02Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:147 | no match found for 'CVACT02Y' |
| `CVACT03Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:150 | no match found for 'CVACT03Y' |
| `CVCUS01Y` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:153 | no match found for 'CVCUS01Y' |
| `DFHAID` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:168 | no match found for 'DFHAID' |
| `DFHBMSCA` | copybook | /Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:169 | no match found for 'DFHBMSCA' |
