# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:35:05.303135

## Purpose

This PSB source file defines the IMS database access structure for the COBOL program PAUTBUNL. It specifies a single DB-type PCB named PAUTBPCB accessing IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14 (lines 18). The SENSGs define PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as its child segment under PAUTSUM0 (lines 19-20). The PSB is generated for COBOL language use (line 21).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database name accessed via DB PCB PAUTBPCB with PROCOPT=GOTP and KEYLEN=14 |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) accessible via the PCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment with PARENT=PAUTSUM0, accessible via the PCB |
