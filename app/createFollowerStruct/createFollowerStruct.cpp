/** \file
 * \brief Example code for Simple Open EtherCAT master
 *
 * Usage : followerinfo [ifname] [-sdo] [-map]
 * Ifname is NIC interface, f.e. eth0.
 * Optional -sdo to display CoE object dictionary.
 * Optional -map to display follower PDO mapping
 *
 * This shows the configured follower data.
 *
 * (c)Arthur Ketels 2010 - 2011
 */

//-----------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <ecat2String.h>
#include "ethercat.h"

//-----------------------------------------------------------------------------
char IOmap[4096];
ec_ODlistt ODlist;
ec_OElistt OElist;
boolean printSDO = FALSE;
boolean printMAP = FALSE;
boolean printCyclic = FALSE;
boolean printAcyclic = FALSE;


/** Read PDO assign structure */
int si_PDOassign(uint16 follower, uint16 PDOassign, int mapoffset, int bitoffset)
{
    uint16 idxloop, nidx, subidxloop, rdat, idx, subidx;
    uint8 subcnt;
    int wkc, bsize = 0, rdl;
    int32 rdat2;
    uint8 bitlen, obj_subidx;
    uint16 obj_idx;
    int abs_offset, abs_bit;

    rdl = sizeof(rdat); rdat = 0;
    /* read PDO assign subindex 0 ( = number of PDO's) */
    wkc = ec_SDOread(follower, PDOassign, 0x00, FALSE, &rdl, &rdat, EC_TIMEOUTRXM);
    rdat = etohs(rdat);
    /* positive result from follower ? */
    if ((wkc > 0) && (rdat > 0))
    {
        /* number of available sub indexes */
        nidx = rdat;
        bsize = 0;
        /* read all PDO's */
        for (idxloop = 1; idxloop <= nidx; idxloop++)
        {
            rdl = sizeof(rdat); rdat = 0;
            /* read PDO assign */
            wkc = ec_SDOread(follower, PDOassign, (uint8)idxloop, FALSE, &rdl, &rdat, EC_TIMEOUTRXM);
            /* result is index of PDO */
            idx = etohs(rdat);
            if (idx > 0)
            {
                rdl = sizeof(subcnt); subcnt = 0;
                /* read number of subindexes of PDO */
                wkc = ec_SDOread(follower,idx, 0x00, FALSE, &rdl, &subcnt, EC_TIMEOUTRXM);
                subidx = subcnt;
                /* for each subindex */
                for (subidxloop = 1; subidxloop <= subidx; subidxloop++)
                {
                    rdl = sizeof(rdat2); rdat2 = 0;
                    /* read SDO that is mapped in PDO */
                    wkc = ec_SDOread(follower, idx, (uint8)subidxloop, FALSE, &rdl, &rdat2, EC_TIMEOUTRXM);
                    rdat2 = etohl(rdat2);
                    /* extract bitlength of SDO */
                    bitlen = LO_BYTE(rdat2);
                    bsize += bitlen;
                    obj_idx = (uint16)(rdat2 >> 16);
                    obj_subidx = (uint8)((rdat2 >> 8) & 0x000000ff);
                    abs_offset = mapoffset + (bitoffset / 8);
                    abs_bit = bitoffset % 8;
                    ODlist.Slave = follower;
                    ODlist.Index[0] = obj_idx;
                    OElist.Entries = 0;
                    wkc = 0;
                    /* read object entry from dictionary if not a filler (0x0000:0x00) */
                    if(obj_idx || obj_subidx)
                        wkc = ec_readOEsingle(0, obj_subidx, &ODlist, &OElist);
                    printf("  [0x%4.4X.%1d] 0x%4.4X:0x%2.2X 0x%2.2X", abs_offset, abs_bit, obj_idx, obj_subidx, bitlen);
                    if((wkc > 0) && OElist.Entries)
                    {
                        printf(" %-12s %s\n", dtype2string(OElist.DataType[obj_subidx]), OElist.Name[obj_subidx]);
                    }
                    else
                        printf("\n");
                    bitoffset += bitlen;
                };
            };
        };
    };
    /* return total found bitlength (PDO) */
    return bsize;
}

int si_map_sdo(int follower)
{
    int wkc, rdl;
    int retVal = 0;
    uint8 nSM, iSM, tSM;
    int Tsize, outputs_bo, inputs_bo;
    uint8 SMt_bug_add;

    printf("PDO mapping according to CoE :\n");
    SMt_bug_add = 0;
    outputs_bo = 0;
    inputs_bo = 0;
    rdl = sizeof(nSM); nSM = 0;
    /* read SyncManager Communication Type object count */
    wkc = ec_SDOread(follower, ECT_SDO_SMCOMMTYPE, 0x00, FALSE, &rdl, &nSM, EC_TIMEOUTRXM);
    /* positive result from follower ? */
    if ((wkc > 0) && (nSM > 2))
    {
        /* make nSM equal to number of defined SM */
        nSM--;
        /* limit to maximum number of SM defined, if true the follower can't be configured */
        if (nSM > EC_MAXSM)
            nSM = EC_MAXSM;
        /* iterate for every SM type defined */
        for (iSM = 2 ; iSM <= nSM ; iSM++)
        {
            rdl = sizeof(tSM); tSM = 0;
            /* read SyncManager Communication Type */
            wkc = ec_SDOread(follower, ECT_SDO_SMCOMMTYPE, iSM + 1, FALSE, &rdl, &tSM, EC_TIMEOUTRXM);
            if (wkc > 0)
            {
                if((iSM == 2) && (tSM == 2)) // SM2 has type 2 == mailbox out, this is a bug in the follower!
                {
                    SMt_bug_add = 1; // try to correct, this works if the types are 0 1 2 3 and should be 1 2 3 4
                    printf("Activated SM type workaround, possible incorrect mapping.\n");
                }
                if(tSM)
                    tSM += SMt_bug_add; // only add if SMt > 0

                if (tSM == 3) // outputs
                {
                    /* read the assign RXPDO */
                    printf("  SM%1d outputs\n     addr b   index: sub bitl data_type    name\n", iSM);
                    Tsize = si_PDOassign(follower, ECT_SDO_PDOASSIGN + iSM, (int)(ec_slave[follower].outputs - (uint8 *)&IOmap[0]), outputs_bo );
                    outputs_bo += Tsize;
                }
                if (tSM == 4) // inputs
                {
                    /* read the assign TXPDO */
                    printf("  SM%1d inputs\n     addr b   index: sub bitl data_type    name\n", iSM);
                    Tsize = si_PDOassign(follower, ECT_SDO_PDOASSIGN + iSM, (int)(ec_slave[follower].inputs - (uint8 *)&IOmap[0]), inputs_bo );
                    inputs_bo += Tsize;
                }
            }
        }
    }

    /* found some I/O bits ? */
    if ((outputs_bo > 0) || (inputs_bo > 0))
        retVal = 1;
    return retVal;
}

int si_siiPDO(uint16 follower, uint8 t, int mapoffset, int bitoffset)
{
    uint16 a , w, c, e, er, Size;
    uint8 eectl;
    uint16 obj_idx;
    uint8 obj_subidx;
    uint8 obj_name;
    uint8 obj_datatype;
    uint8 bitlen;
    int totalsize;
    ec_eepromPDOt eepPDO;
    ec_eepromPDOt *PDO;
    int abs_offset, abs_bit;
    char str_name[EC_MAXNAME + 1];

    eectl = ec_slave[follower].eep_pdi;
    Size = 0;
    totalsize = 0;
    PDO = &eepPDO;
    PDO->nPDO = 0;
    PDO->Length = 0;
    PDO->Index[1] = 0;
    for (c = 0 ; c < EC_MAXSM ; c++) PDO->SMbitsize[c] = 0;
    if (t > 1)
        t = 1;
    PDO->Startpos = ec_siifind(follower, ECT_SII_PDO + t);
    if (PDO->Startpos > 0)
    {
        a = PDO->Startpos;
        w = ec_siigetbyte(follower, a++);
        w += (ec_siigetbyte(follower, a++) << 8);
        PDO->Length = w;
        c = 1;
        /* traverse through all PDOs */
        do
        {
            PDO->nPDO++;
            PDO->Index[PDO->nPDO] = ec_siigetbyte(follower, a++);
            PDO->Index[PDO->nPDO] += (ec_siigetbyte(follower, a++) << 8);
            PDO->BitSize[PDO->nPDO] = 0;
            c++;
            /* number of entries in PDO */
            e = ec_siigetbyte(follower, a++);
            PDO->SyncM[PDO->nPDO] = ec_siigetbyte(follower, a++);
            a++;
            obj_name = ec_siigetbyte(follower, a++);
            a += 2;
            c += 2;
            if (PDO->SyncM[PDO->nPDO] < EC_MAXSM) /* active and in range SM? */
            {
                str_name[0] = 0;
                if(obj_name)
                  ec_siistring(str_name, follower, obj_name);
                if (t)
                  printf("  SM%1d RXPDO 0x%4.4X %s\n", PDO->SyncM[PDO->nPDO], PDO->Index[PDO->nPDO], str_name);
                else
                  printf("  SM%1d TXPDO 0x%4.4X %s\n", PDO->SyncM[PDO->nPDO], PDO->Index[PDO->nPDO], str_name);
                printf("     addr b   index: sub bitl data_type    name\n");
                /* read all entries defined in PDO */
                for (er = 1; er <= e; er++)
                {
                    c += 4;
                    obj_idx = ec_siigetbyte(follower, a++);
                    obj_idx += (ec_siigetbyte(follower, a++) << 8);
                    obj_subidx = ec_siigetbyte(follower, a++);
                    obj_name = ec_siigetbyte(follower, a++);
                    obj_datatype = ec_siigetbyte(follower, a++);
                    bitlen = ec_siigetbyte(follower, a++);
                    abs_offset = mapoffset + (bitoffset / 8);
                    abs_bit = bitoffset % 8;

                    PDO->BitSize[PDO->nPDO] += bitlen;
                    a += 2;

                    /* skip entry if filler (0x0000:0x00) */
                    if(obj_idx || obj_subidx)
                    {
                       str_name[0] = 0;
                       if(obj_name)
                          ec_siistring(str_name, follower, obj_name);

                       printf("  [0x%4.4X.%1d] 0x%4.4X:0x%2.2X 0x%2.2X", abs_offset, abs_bit, obj_idx, obj_subidx, bitlen);
                       printf(" %-12s %s\n", dtype2string(obj_datatype), str_name);
                    }
                    bitoffset += bitlen;
                    totalsize += bitlen;
                }
                PDO->SMbitsize[ PDO->SyncM[PDO->nPDO] ] += PDO->BitSize[PDO->nPDO];
                Size += PDO->BitSize[PDO->nPDO];
                c++;
            }
            else /* PDO deactivated because SM is 0xff or > EC_MAXSM */
            {
                c += 4 * e;
                a += 8 * e;
                c++;
            }
            if (PDO->nPDO >= (EC_MAXEEPDO - 1)) c = PDO->Length; /* limit number of PDO entries in buffer */
        }
        while (c < PDO->Length);
    }
    if (eectl) ec_eeprom2pdi(follower); /* if eeprom control was previously pdi then restore */
    return totalsize;
}


int si_map_sii(int follower)
{
    int retVal = 0;
    int Tsize, outputs_bo, inputs_bo;

    printf("PDO mapping according to SII :\n");

    outputs_bo = 0;
    inputs_bo = 0;
    /* read the assign RXPDOs */
    Tsize = si_siiPDO(follower, 1, (int)(ec_slave[follower].outputs - (uint8*)&IOmap), outputs_bo );
    outputs_bo += Tsize;
    /* read the assign TXPDOs */
    Tsize = si_siiPDO(follower, 0, (int)(ec_slave[follower].inputs - (uint8*)&IOmap), inputs_bo );
    inputs_bo += Tsize;
    /* found some I/O bits ? */
    if ((outputs_bo > 0) || (inputs_bo > 0))
        retVal = 1;
    return retVal;
}

void si_sdo(int cnt)
{
    int i, j;

    ODlist.Entries = 0;
    memset(&ODlist, 0, sizeof(ODlist));
    if( ec_readODlist(cnt, &ODlist))
    {
        printf(" CoE Object Description found, %d entries.\n",ODlist.Entries);
        for( i = 0 ; i < ODlist.Entries ; i++)
        {
            ec_readODdescription(i, &ODlist);
            while(EcatError) printf("%s", ec_elist2string());
            //printf(" Index: %4.4x Datatype: %4.4x Objectcode: %2.2x Name: %s\n",
            //    ODlist.Index[i], ODlist.DataType[i], ODlist.ObjectCode[i], ODlist.Name[i]);
            
            //printf(" 0x%4.4x , %s\n", ODlist.Index[i], ODlist.Name[i]);


            memset(&OElist, 0, sizeof(OElist));
            ec_readOE(i, &ODlist, &OElist);
            while(EcatError) printf("%s", ec_elist2string());
            for( j = 0 ; j < ODlist.MaxSub[i]+1 ; j++)
            {
                if ((OElist.DataType[j] > 0) && (OElist.BitLength[j] > 0))
                {
                    //printf("  Sub: %2.2x Datatype: %4.4x Bitlength: %4.4x Obj.access: %4.4x Name: %s\n",
                    //    j, OElist.DataType[j], OElist.BitLength[j], OElist.ObjAccess[j], OElist.Name[j]);

                    if(printAcyclic && ( ODlist.Index[i] > 0x1FFF))
                    {
                        printf("ObjectEntry %s_%s { 0x%4.4x, %2.2x, %4.4x, 0}", ODlist.Name[i], OElist.Name[j],
                        ODlist.Index[i], j,  OElist.DataType[j]);
                        printf("\n");
                    }
                    else
                    {
                        //printf("ObjectEntry %s_%s { 0x%4.4x, %2.2x, %4.4x, 0} // Bitlength: %4.4x Obj.access: %4.4x ", ODlist.Name[i], OElist.Name[j],
                        //ODlist.Index[i], j,  OElist.DataType[j], OElist.BitLength[j], OElist.ObjAccess[j]);
                    }

                    
                
                    if ((OElist.ObjAccess[j] & 0x0007) && !printAcyclic)
                    {
                        printf("  Value :%s\n", SDO2string(cnt, ODlist.Index[i], j, OElist.DataType[j]));
                    }
                }

            }

        }
    }
    else
    {
        while(EcatError) printf("%s", ec_elist2string());
    }
}

void followerinfo(char *ifname)
{
   int cnt, i, j, nSM;
    uint16 ssigen;
    int expectedWKC;

   printf("Starting followerinfo\n");

   /* initialise SOEM, bind socket to ifname */
   if (ec_init(ifname))
   {
      printf("ec_init on %s succeeded.\n",ifname);
      /* find and auto-config followers */
      if ( ec_config(FALSE, &IOmap) > 0 )
      {
         ec_configdc();
         while(EcatError) printf("%s", ec_elist2string());
         printf("%d followers found and configured.\n",ec_slavecount);
         expectedWKC = (ec_group[0].outputsWKC * 2) + ec_group[0].inputsWKC;
         printf("Calculated workcounter %d\n", expectedWKC);
         /* wait for all followers to reach SAFE_OP state */
         ec_statecheck(0, EC_STATE_SAFE_OP,  EC_TIMEOUTSTATE * 3);
         if (ec_slave[0].state != EC_STATE_SAFE_OP )
         {
            printf("Not all followers reached safe operational state.\n");
            ec_readstate();
            for(i = 1; i<=ec_slavecount ; i++)
            {
               if(ec_slave[i].state != EC_STATE_SAFE_OP)
               {
                  printf("follower %d State=%2x StatusCode=%4x : %s\n",
                     i, ec_slave[i].state, ec_slave[i].ALstatuscode, ec_ALstatuscode2string(ec_slave[i].ALstatuscode));
               }
            }
         }


         ec_readstate();
         for( cnt = 1 ; cnt <= ec_slavecount ; cnt++)
         {
            printf("\nfollower:%d\n Name:%s\n Output size: %dbits\n Input size: %dbits\n State: %d\n Delay: %d[ns]\n Has DC: %d\n",
                  cnt, ec_slave[cnt].name, ec_slave[cnt].Obits, ec_slave[cnt].Ibits,
                  ec_slave[cnt].state, ec_slave[cnt].pdelay, ec_slave[cnt].hasdc);
            if (ec_slave[cnt].hasdc) printf(" DCParentport:%d\n", ec_slave[cnt].parentport);
            printf(" Activeports:%d.%d.%d.%d\n", (ec_slave[cnt].activeports & 0x01) > 0 ,
                                         (ec_slave[cnt].activeports & 0x02) > 0 ,
                                         (ec_slave[cnt].activeports & 0x04) > 0 ,
                                         (ec_slave[cnt].activeports & 0x08) > 0 );
            printf(" Configured address: %4.4x\n", ec_slave[cnt].configadr);
            printf(" Man: %8.8x ID: %8.8x Rev: %8.8x\n", (int)ec_slave[cnt].eep_man, (int)ec_slave[cnt].eep_id, (int)ec_slave[cnt].eep_rev);
            for(nSM = 0 ; nSM < EC_MAXSM ; nSM++)
            {
               if(ec_slave[cnt].SM[nSM].StartAddr > 0)
                  printf(" SM%1d A:%4.4x L:%4d F:%8.8x Type:%d\n",nSM, etohs(ec_slave[cnt].SM[nSM].StartAddr), etohs(ec_slave[cnt].SM[nSM].SMlength),
                         etohl(ec_slave[cnt].SM[nSM].SMflags), ec_slave[cnt].SMtype[nSM]);
            }
            for(j = 0 ; j < ec_slave[cnt].FMMUunused ; j++)
            {
               printf(" FMMU%1d Ls:%8.8x Ll:%4d Lsb:%d Leb:%d Ps:%4.4x Psb:%d Ty:%2.2x Act:%2.2x\n", j,
                       etohl(ec_slave[cnt].FMMU[j].LogStart), etohs(ec_slave[cnt].FMMU[j].LogLength), ec_slave[cnt].FMMU[j].LogStartbit,
                       ec_slave[cnt].FMMU[j].LogEndbit, etohs(ec_slave[cnt].FMMU[j].PhysStart), ec_slave[cnt].FMMU[j].PhysStartBit,
                       ec_slave[cnt].FMMU[j].FMMUtype, ec_slave[cnt].FMMU[j].FMMUactive);
            }
            printf(" FMMUfunc 0:%d 1:%d 2:%d 3:%d\n",
                     ec_slave[cnt].FMMU0func, ec_slave[cnt].FMMU1func, ec_slave[cnt].FMMU2func, ec_slave[cnt].FMMU3func);
            printf(" MBX length wr: %d rd: %d MBX protocols : %2.2x\n", ec_slave[cnt].mbx_l, ec_slave[cnt].mbx_rl, ec_slave[cnt].mbx_proto);
            ssigen = ec_siifind(cnt, ECT_SII_GENERAL);
            /* SII general section */
            if (ssigen)
            {
               ec_slave[cnt].CoEdetails = ec_siigetbyte(cnt, ssigen + 0x07);
               ec_slave[cnt].FoEdetails = ec_siigetbyte(cnt, ssigen + 0x08);
               ec_slave[cnt].EoEdetails = ec_siigetbyte(cnt, ssigen + 0x09);
               ec_slave[cnt].SoEdetails = ec_siigetbyte(cnt, ssigen + 0x0a);
               if((ec_siigetbyte(cnt, ssigen + 0x0d) & 0x02) > 0)
               {
                  ec_slave[cnt].blockLRW = 1;
                  ec_slave[0].blockLRW++;
               }
               ec_slave[cnt].Ebuscurrent = ec_siigetbyte(cnt, ssigen + 0x0e);
               ec_slave[cnt].Ebuscurrent += ec_siigetbyte(cnt, ssigen + 0x0f) << 8;
               ec_slave[0].Ebuscurrent += ec_slave[cnt].Ebuscurrent;
            }
            printf(" CoE details: %2.2x FoE details: %2.2x EoE details: %2.2x SoE details: %2.2x\n",
                    ec_slave[cnt].CoEdetails, ec_slave[cnt].FoEdetails, ec_slave[cnt].EoEdetails, ec_slave[cnt].SoEdetails);
            printf(" Ebus current: %d[mA]\n only LRD/LWR:%d\n",
                    ec_slave[cnt].Ebuscurrent, ec_slave[cnt].blockLRW);
            if ((ec_slave[cnt].mbx_proto & ECT_MBXPROT_COE) && printSDO){
                    si_sdo(cnt);}
            if ((ec_slave[cnt].mbx_proto & ECT_MBXPROT_COE) && printAcyclic){
                    si_sdo(cnt);}
            
            if(printMAP){
                if (ec_slave[cnt].mbx_proto & ECT_MBXPROT_COE){
                    si_map_sdo(cnt);}
                else{
                    si_map_sii(cnt);}
            }
         }
      }
      else
      {
         printf("No followers found!\n");
      }
      printf("End followerinfo, close socket\n");
      /* stop SOEM, close socket */
      ec_close();
   }
   else
   {
      printf("No socket connection on %s\nExcecute as root\n",ifname);
   }
}

char ifbuf[1024];

int main(int argc, char *argv[])
{
   ec_adaptert * adapter = NULL;
   printf("SOEM (Simple Open EtherCAT Master)\nfollowerinfo\n");

   if (argc > 1)
   {
      if ((argc > 2) && (strncmp(argv[2], "-sdo", sizeof("-sdo")) == 0)) printSDO = TRUE;
      if ((argc > 2) && (strncmp(argv[2], "-map", sizeof("-map")) == 0)) printMAP = TRUE;
      if ((argc > 2) && (strncmp(argv[2], "-cyclic", sizeof("-cyclic")) == 0)) printCyclic = TRUE;
      if ((argc > 2) && (strncmp(argv[2], "-acyclic", sizeof("-acyclic")) == 0)) printAcyclic = TRUE;
      /* start followerinfo */
      strcpy(ifbuf, argv[1]);
      followerinfo(ifbuf);
   }
   else
   {
      printf("Usage: followerinfo ifname [options]\nifname = eth0 for example\nOptions :\n");
      printf(" -sdo : print SDO info\n");
      printf(" -map : print mapping\n");
      printf(" -cyclic : prints Cyclic Structure\n");
      printf(" -acyclic : prints Acyclic Structure\n");

      printf ("Available adapters\n");
      adapter = ec_find_adapters ();
      while (adapter != NULL)
      {
         printf ("Description : %s, Device to use for wpcap: %s\n", adapter->desc,adapter->name);
         adapter = adapter->next;
      }
      ec_free_adapters(adapter);
   }

   printf("End program\n");
   return (0);
}
