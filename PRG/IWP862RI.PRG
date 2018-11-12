*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP862RI.PRG -> Generacion de archivos REGIMEN INFORMACION COMPRAS
*)-H
*)-H Release         : 16
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    : 
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_pfile='862'
w_title='SIAP - REGIMEN DE INFORMACION - '+upper(prompt())

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'	
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F12'
fieldvec[9]='F23'

use (d0f00) in 0 alias 'f00'												
use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
use (d0f12) in 0 alias 'f12' 		  		&&(Parametros de Sistema)	
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

go bott in f04
go top in f12

wdici		= f12.p0diri
wcoto		= 'N'

wtrim		= '1'
wano		= right(f04->d4peri,4)


wdperi		= f04.d4peri
whperi		= f04.d4peri

wficobi			= 'C'+d0coem+right(wdperi,2)+left(wdperi,2)+'C.TXT'

w_ficobi	= wficobi

w_modo		= 1		&& R.03 Mariano (1-por comprobante / 2-Global)

w_fere		= 'N'	&& R.04 Mariano (Fechas reales (S/N))

do ssm998.mpr

do iws862ri.spr		&& R.03 Mariano (antes do iws862.spr)

pop key
pop menu _msysmenu
=closall()
DELE FILE &d0f99.c.dbf
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
			wdperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wdperi
	ELSE
	    IF !w_show
			WAIT WIND "El perํodo no es correcto"
	    ENDIF
	    w_retu	= .F.
	    _curobj	= objnum(wdperi)
	    RETURN .F.
	ENDIF
	wkperi = ptoi(ALLT(wdperi))
	if ! seek(wkperi,'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
	endif

	IF w_retu
			wficobi	= 'C'+d0coem+right(wdperi,2)+left(wdperi,2)+'C.TXT'
			whperi		= wdperi
	ELSE
		wficobi	= "ERROR.TXT"
	ENDIF
	SHOW GETS
endif

		
w_dperi=ptoi(wdperi)
w_hperi=ptoi(whperi)
		
show gets

if wdperi>whperi
	w_retu=.f.
endif

if w_vari='WDICI' or w_show
	w_DiCi=alltrim(wDiCi)
	wDiCi =subs(iif(subs(w_DiCi,len(w_DiCi),1)<>'\',w_DiCi+'\',w_DiCi)+spac(20),1,20)
	w_DiCi=alltrim(wDiCi)	
	if !mfile(w_DiCi+'NUL')
		if sino('Desea crear el directorio -> '+w_dici)='S'
			if mkdir(left(w_dici,len(w_dici)-1)) = 0
	  			=advsuave('Creo el directorio -> '+w_dici)
			else
				=advgrave('No pudo crear el directorio -> '+w_dici)
				w_retu=.f.
			endif
		else
			=advgrave('No existe el directorio -> '+w_dici)
			w_retu=.f.
		endif
	else
		w_FIcobi=w_dici+alltrim(wFIcobi)
	endif
endif

if w_vari='WFICOBI' or w_show
	if empty(wficobi)
		if !w_show
			wait wind 'Debe especificar el nombre del archivo de compras.'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WCOTO' or w_show
	if !wcoto$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons862x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons862x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid862(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Generaci๓n ?.')='S'
	wait wind nowait 'Creando Archivo Temporario...'

	if creamov(wDperi,wHperi,'C','1',.t.,.t.)
		wait clear
		if !eof('f9c')
		
			dele file &w_FIcobi
			
			if Sele860(wCoTo)
				sele f9c

				DELE ALL FOR d5tipo = 'C'	AND d5tico = 'R'			&& (Elimino retenciones)

*				dele for !d5coco$'001.002.003.004.005.008.013.030.034.037.039.042.050.051.052.053.054.055.056.057.058.059.060.063.070.081.084.086.099.110.112.113.114.115.118.119.120'	&& R.01 Mariano (Agregue 099-Otro no cumple..)	&& R.02 Mariano (mando todos)

				dele tag all
				inde on d5sele+d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco tag d5sele

				wait wind nowait 'Generando Compras...'
				=GeAr862('C',w_FIcobi)
				wait clear
				=advsuave('Fin del Proceso, Archivo compras generado.','Informaci๓n')
			else
				=advsuave('No Hay datos que Cumplan...','Informaci๓n')
			endif
			
		else
			=advsuave('No hay datos...','Informaci๓n')
		endif
		sele f9c
		use
		dele file &d0f99.c.dbf
		dele file &d0f99.c.cdx
	endif
	wait clear
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC gear862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_tipo,s_file

IF USED('f99d')
		USE in f99d
		DELE FILE &d0f99.d.dbf
ENDIF		

SELE 0

CREATE dbf &d0f99.d (d99reng C(254), d99line L)
USE

USE &d0f99.d IN 0 ALIAS f99d EXCL

set alternate to &s_file
set console off
set alternate on

sele f9c
if start('*'+S_tipo,'f9c')	

	w_toti=0	&& R.05 Mariano

	do while d5sele+d5tipo='*'+S_tipo
	
	  store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (agregue w5imp5)

	  store '' to w5cpga,w5cpib	&& R.08 Mariano
	  	  
      w5tido=d5tido
      w5cuit=d5cuit

      w5peri=d5peri
      w5tiop=d5tiop
	  w5coco=d5coco
	  
		w5nume	= d5nume	     
*		w5fech	= d5fech	&& R.04 Mariano
		w5fech	= iif(w_fere='S' or d5fech>=ctod('01/'+d5peri),d5fech,ctod('01/'+d5peri))	&& R.04 Mariano
		w5clie	= d5clie	  	  

		dime alic(21,3)

		alic=0
	
      do while d5sele+d5tipo='*'+S_tipo ;
         and w5clie=d5clie;
         and w5peri=d5peri;
         and w5tiop=d5tiop;
         and w5coco=d5coco;
         AND w5nume	= d5nume
         
         
*			w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')	&& R.02 Mariano
			w5alic=iif(seek(d5alic,'F23'),f23.d23alic,0)	&& R.02 Mariano
			i=val(d5alic)+1
			if !empty(d5imp1) and empty(d5imp4)	
*				w5alic='0000'	&& R.02 Mariano
				w5alic=0	&& R.02 Mariano
				i=1
			endif
			if !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano
				alic(i,1)=w5alic
				alic(i,2)=alic(i,2)+d5imp4
				alic(i,3)=alic(i,3)+d5imp1
			endif	&& R.06 Mariano

			
			w5tota = w5tota + d5tota
			w5neto = w5neto + d5imp1
			w5impu = w5impu + d5imp4

			w5imp3= w5imp3 + d5imp3
			w5imp5= w5imp5 + d5imp5	&& R.06 Mariano
			w5imp6= w5imp6 + d5imp6
			w5imp7= w5imp7 + d5imp7
			w5imp8= w5imp8 + d5imp8

			&& R.08b Mariano
			if !empty(d5cpga)
				w5cpga=d5cpga
			endif
			if !empty(d5cpib)
				w5cpib=d5cpib
			endif
			&& R.08e Mariano

			if !empty(d5imp2)
				if d5imp2=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
				endif
			endif
			
			if empty(w5tiop) and !empty(d5tiop)
				w5tiop=d5tiop
			endif

         
         skip
                 
      enddo

	  skip -1 

&& R.09b Mariano
*  		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')),30)
*		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f03'), f03.d3tido, '  '))

*		w5nuic=iif(!d5cuit='  -        - ',left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1),'00000000000')
		w5nocl=d5nocl
		w5tido=d5tido
		w5nuic=strtran(d5cuit,'-','')
&& R.09e Mariano
		
		if val(w5nuic)=0
			w5tido='99'
		endif

		for i=1 to 21
			if !empty(alic(i,1))
				w5caal=w5caal+1
			endif
		endfor

		cont_reg=1

		
		for i=1 to 21
			if !empty(alic(i,1)) or (w5caal=0 and i=21)

				if w5caal=0 and !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano (agregue que no sea 083-tiquet sin letra)
					w5caal=1
				endif

*				if cont_reg=w5caal 									&& R.02 Mariano
*				if cont_reg=w5caal or (w5caal=0 and d5tifo$'B.C')	&& R.02 Mariano	&& R.06 Mariano
				if cont_reg=w5caal or (w5caal=0 and (d5tifo$'B.C' or d5coco='083'))	&& R.06 Mariano

					?? dtos(iif(w_fere='S' or d5fech>=ctod('01/'+d5peri),d5fech,ctod('01/'+d5peri)))	&& R.04 Mariano (w_fere='S')
					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? padl(allt(left(d5nume,4)),5,'0')									&& R.01 Mariano
*					?? iif(val(d5coco)=99,space(5),padl(allt(left(d5nume,4)),5,'0'))	&& R.01 Mariano	&& R.02 Mariano
*					?? iif(val(d5coco)=99,'00000',padl(allt(left(d5nume,4)),5,'0'))		&& R.02 Mariano	&& R.05 Mariano
*					?? iif(val(d5coco)=99 or val(d5coco)=33 or val(d5coco)=66,'00000',padl(allt(left(d5nume,4)),5,'0'))	&& R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66 or at('-',d5nume)=0, '00000', padl(allt(left(d5nume,min(5,at('-',d5nume)-1))),5,'0'))	&& R.06 Mariano
*					?? padl(allt(right(d5nume,8)),20,'0')  								&& R.01 Mariano
*					?? padl(allt(iif(val(d5coco)=99,d5nume,right(d5nume,8))),20,'0')  	&& R.01 Mariano	&& R.05 Mariano
*					?? iif(val(d5coco)=66, repli('0',20),padl(allt(iif(val(d5coco)=99 or val(d5coco)=33,d5nume,right(d5nume,8))),20,'0'))  && R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66, repli('0',20), padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),20,'0'))  && R.06 Mariano
					?? iif(!empty(d5adua),right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf,repl(' ',16))		&& Despacho importacion
					?? w5tido
					?? padl(allt(w5nuic),20,'0')
					?? LEFT(w5nocl, 30)
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(w5tota*100),15)),15,'0')	&& total							&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*((iif(d5tifo$'B.C' or d5coco='083',w5neto,0)+w5otros)*100),15)),15,'0')	&& no gravados	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )	&& R.07 Mariano
					&& R.11b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083',w5otros,0)*100),15)),15,'0')	&& no gravados	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )	&& R.07 Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083',w5exen,0)*100),15)),15,'0')	&& exento		&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5imp3+percga())*100),15)),15,'0')	&& percepciones nacionales (iva + ganancias)	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str((iif(w5tota<0,(-1),1)*(percot())*100),15)),15,'0')	&& percepciones nacionales (otros)	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(percib()*100),15)),15,'0')	&& percepciones ing.brutos			&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and w5otros>0) or (w5tota>0 and w5otros<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(iif(!d5tifo$'B.C' and d5coco!='083',w5otros,0)*100),14)),14,'0')	&& no gravados
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083',w5otros,0)*100),15)),15,'0')	&& no gravados
					endif
					if (w5tota<0 and w5exen>0) or (w5tota>0 and w5exen<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(iif(!d5tifo$'B.C' and d5coco!='083',w5exen,0)*100),14)),14,'0')	&& exento	
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083',w5exen,0)*100),15)),15,'0')	&& exento	
					endif
					if (w5tota<0 and w5imp3>0) or (w5tota>0 and w5imp3<0)
						?? '-'+padl(allt(str((iif(w5tota<0,1,(-1))*(w5imp3)*100),14)),14,'0')	&& percepciones nacionales (iva)
					else
						?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5imp3)*100),15)),15,'0')	&& percepciones nacionales (iva)
					endif
					if (w5tota<0 and percga()+percot()>0) or (w5tota>0 and percga()+percot()<0)
						?? '-'+padl(allt(str((iif(w5tota<0,1,(-1))*(percga()+percot())*100),14)),14,'0')	&& percepciones nacionales (ganancias + otros)
					else
						?? padl(allt(str((iif(w5tota<0,(-1),1)*(percga()+percot())*100),15)),15,'0')	&& percepciones nacionales (ganancias + otros)
					endif
					if (w5tota<0 and percib()>0) or (w5tota>0 and percib()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(percib()*100),14)),14,'0')	&& percepciones ing.brutos	&& R.15 Mariano (faltaba '-'+ al principio)
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(percib()*100),15)),15,'0')	&& percepciones ing.brutos
					endif
					&& R.11e Mariano
					&& R.15b Mariano
*					?? '000000000000000'	&& percepciones municipales
					if (w5tota<0 and impmun()>0) or (w5tota>0 and impmun()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(impmun()*100),14)),14,'0')	&& impuestos municipales
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(impmun()*100),15)),15,'0')	&& impuestos municipales
					endif
					&& R.15e Mariano
					&& R.11b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(impint()*100),15)),15,'0')	&& impuestos internos				&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and impint()>0) or (w5tota>0 and impint()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(impint()*100),14)),14,'0')	&& impuestos internos	&& R.15 Mariano (faltaba '-'+ al principio y 14 x15)
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(impint()*100),15)),15,'0')	&& impuestos internos
					endif
					&& R.11e Mariano
					?? 'PES'	&& codigo de moneda
					?? '0001000000'	&& tipo de cambio
					?? str(w5caal,1)	&& cantidad de alicuotas
					?? iif(!empty(w5tiop),w5tiop, iif(empty(w5neto), iif(empty(alic(i,1)),'N','E'),''))		&& codigo operacion
					&& R.11b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083' and w_modo=1,w5impu,0))*100,15)),15,'0')		&& Iva	&& R.03 Mariano (and w_modo=1)	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(otrib()*100),15)),15,'0')	&& otros tributos					&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and w5impu>0) or (w5tota>0 and w5impu<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(iif(!d5tifo$'B.C' and d5coco!='083' and w_modo=1,w5impu,0))*100,14)),14,'0')		&& Iva
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(iif(!d5tifo$'B.C' and d5coco!='083' and w_modo=1,w5impu,0))*100,15)),15,'0')		&& Iva
					endif
					if (w5tota<0 and otrib()>0) or (w5tota>0 and otrib()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(otrib()*100),14)),14,'0')	&& otros tributos	
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(otrib()*100),15)),15,'0')	&& otros tributos	
					endif
					&& R.11e Mariano

					&& R.12b Jonatan
*					?? '00000000000'	&& cuit vendedor
*					?? SPACE(30)		&& nombre vendedor
*					?? '000000000000000'	&& iva comision
*					?? iif(!d5cuco!='  -        - ',left(d5cuco,2)+substr(d5cuco,4,8)+right(d5cuco,1),'00000000000') && cuit vendedor	&& R.14 Mariano
					?? iif(d5cuco!='  -        - ' and !empty(d5cuco),left(d5cuco,2)+substr(d5cuco,4,8)+right(d5cuco,1),'00000000000') && cuit vendedor	&& R.14 Mariano
					?? left(d5deco,30) && nombre vendedor	&& R.13 Mariano (limite a 30)
					??	padl(allt(str(iif(w5tota<0,(-1),1)*(d5ivco*100),15)),15,'0') && iva comision	&& R.12 Mariano (d5ivco x ivco)
					&& R.12e Jonatan
 					?

					w_toti=w_toti+w5tota	&& R.05 Mariano
					
*					w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)+' '+iif(val(d5coco)=66, space(10)+right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf, iif(val(d5coco)=99 or val(d5coco)=33,'00000',padl(allt(left(d5nume,4)),5,'0')) + ' ' + padl(allt(iif(val(d5coco)=99 or val(d5coco)=33,d5nume,right(d5nume,8))),20,'0')) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 30) + ' ' + str(abs(w5tota),12,2)	&& R.10 Mariano
*					w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)+' '+iif(val(d5coco)=66, space(10)+right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf, padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),20,'0')) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 30) + ' ' + str(abs(w5tota),12,2)	&& R.10 Mariano	&& R.12 Mariano
*					w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)+' '+iif(val(d5coco)=66, space(10)+right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf, padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),20,'0')) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 35) + '  ' + str(abs(w5tota),12,2)	&& R.12 Mariano	&& R.15 Mariano
					w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)+' '+iif(val(d5coco)=66, space(10)+right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf, iif(at('-',d5nume)=0,space(6),padl(allt(left(d5nume,at('-',d5nume)-1)),5,'0')+'-')+padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),14,'0')) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 35) + '  ' + str(abs(w5tota),12,2)	&& R.15 Mariano
					SELE f99d
					APPE BLAN
					REPLA d99reng		WITH w_99reng
					SELE f9c

				else

					cont_reg=cont_reg+1

				endif
			endif
		endfor

		skip
		store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (agregue w5imp5)
		
		store '' to w5cpga,w5cpib	&& R.08 Mariano

	enddo
	
	&& R.05b Mariano
	if reccount('f99d')>0
		w_99reng = space(86)+repli('-',14)
		SELE f99d
		APPE BLAN
		REPLA d99reng		WITH w_99reng
		w_99reng = space(56)+'TOTAL DEL PERIODO : '+space(11)+ str(abs(w_toti),12,2)	
		SELE f99d
		APPE BLAN
		REPLA d99reng		WITH w_99reng
		SELE f9c
	endif
	&& R.05e Mariano
endif
set alternate to
set console on
set alternate off

** archivo de alicuotas **
s_file=strtran(s_file,'C.TXT','A.TXT')
set alternate to &s_file
set console off
set alternate on

sele f9c
if start('*'+S_tipo,'f9c')	

	do while d5sele+d5tipo='*'+S_tipo
		
	  store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (agregue w5imp5)

  	  store '' to w5cpga,w5cpib	&& R.08 Mariano

      w5tido=d5tido
      w5cuit=d5cuit

      w5peri=d5peri
      w5tiop=d5tiop
	  w5coco=d5coco
	  
		w5nume	= d5nume	     
		w5fech	= d5fech   
		w5clie	= d5clie	  	  

		dime alic(21,3)

		alic=0

      do while d5sele+d5tipo='*'+S_tipo ;
         and w5clie=d5clie;
         and w5peri=d5peri;
         and w5tiop=d5tiop;
         and w5coco=d5coco;
         AND w5nume	= d5nume
         
         
			w5alic=iif(seek(d5alic,'F23'),f23.d23alic,0)
			i=val(d5alic)+1
			if !empty(d5imp1) and empty(d5imp4)	
				w5alic=0
				i=1
			endif
			if !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano
				alic(i,1)=w5alic
				alic(i,2)=alic(i,2)+d5imp4
				alic(i,3)=alic(i,3)+d5imp1
			endif	&& R.06 Mariano

			
			w5tota = w5tota + d5tota
			w5neto = w5neto + d5imp1
			w5impu = w5impu + d5imp4

			w5imp3= w5imp3 + d5imp3
			w5imp5= w5imp5 + d5imp5	&& R.06 Mariano
			w5imp6= w5imp6 + d5imp6
			w5imp7= w5imp7 + d5imp7
			w5imp8= w5imp8 + d5imp8

			&& R.08b Mariano
			if !empty(d5cpga)
				w5cpga=d5cpga
			endif
			if !empty(d5cpib)
				w5cpib=d5cpib
			endif
			&& R.08e Mariano

			if !empty(d5imp2)
				if d5imp2=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
				endif
			endif
			
			if empty(w5tiop) and !empty(d5tiop)
				w5tiop=d5tiop
			endif

         
         skip
                 
      enddo

	  skip -1 

&& R.09b Mariano
*  		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')),30)
*		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f03'), f03.d3tido, '  '))

*		w5nuic=iif(!d5cuit='  -        - ',left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1),'00000000000')

		w5nocl=d5nocl
		w5tido=d5tido
		w5nuic=strtran(d5cuit,'-','')
&& R.09e Mariano
		
		if val(w5nuic)=0
			w5tido='99'
		endif

		for i=1 to 21
			if !empty(alic(i,1))
				w5caal=w5caal+1
			endif
		endfor

		cont_reg=1

		for i=1 to 21
*			if !empty(alic(i,1)) and empty(d5adua)	&& no es despacho de importacion	&& R.05 Mariano
*			if ((!empty(alic(i,1)) and w5neto!=0) or !empty(alic(i,3))) and empty(d5adua)	&& no es despacho de importacion 	&& R.05 Mariano	&& R.06 Mariano
			if ( (!empty(alic(i,1)) and w5neto!=0) or (!empty(alic(i,3)) and !d5tifo$'B.C' and d5coco!='083') ) and empty(d5adua)	&& no es despacho de importacion 	&& R.06 Mariano
 
					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? padl(allt(left(d5nume,4)),5,'0')  								&& R.04 Mariano
*					?? iif(val(d5coco)=99,'00000',padl(allt(left(d5nume,4)),5,'0'))		&& R.04 Mariano	&& R.05 Mariano
*					?? iif(val(d5coco)=99 or val(d5coco)=33,'00000',padl(allt(left(d5nume,4)),5,'0'))	&& R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66 or at('-',d5nume)=0,'00000',padl(allt(left(d5nume,min(5,at('-',d5nume)-1))),5,'0'))	&& R.06 Mariano

*					?? padl(allt(right(d5nume,8)),20,'0')  								&& R.04 Mariano
*					?? padl(allt(iif(val(d5coco)=99,d5nume,right(d5nume,8))),20,'0')  	&& R.04 Mariano	&& R.05 Mariano
*					?? padl(allt(iif(val(d5coco)=99 or val(d5coco)=33,d5nume,right(d5nume,8))),20,'0')  && R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66, repli('0',20), padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),20,'0'))  && R.06 Mariano

					?? w5tido
					?? padl(allt(w5nuic),20,'0')
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,3)*100),15)),15,'0')	&& neto gravado	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					?? codalicIVA(alic(i,1))			&& cod. alicuota
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,2)*100),15)),15,'0')	&& iva importe	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
 					?

			endif
		endfor

		if w5caal=0 and !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano (agregue que no sea 083-tiquet sin letra)
			w5caal=1
		endif

*		if w5caal=1 and d5neto=0	and empty(d5adua)	&& R.05 Mariano
		if w5caal=1 and w5neto=0	and empty(d5adua)	&& R.05 Mariano
					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? padl(allt(left(d5nume,4)),5,'0')  								&& R.04 Mariano
*					?? iif(val(d5coco)=99,'00000',padl(allt(left(d5nume,4)),5,'0'))		&& R.04 Mariano	&& R.05 Mariano
*					?? iif(val(d5coco)=99 or val(d5coco)=33,'00000',padl(allt(left(d5nume,4)),5,'0'))	&& R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66 or at('-',d5nume)=0, '00000', padl(allt(left(d5nume,min(5,at('-',d5nume)-1))),5,'0'))	&& R.06 Mariano

*					?? padl(allt(right(d5nume,8)),20,'0')  								&& R.04 Mariano
*					?? padl(allt(iif(val(d5coco)=99,d5nume,right(d5nume,8))),20,'0')  	&& R.04 Mariano	&& R.05 Mariano
*					?? padl(allt(iif(val(d5coco)=99 or val(d5coco)=33,d5nume,right(d5nume,8))),20,'0')  && R.05 Mariano	&& R.06 Mariano
					?? iif(val(d5coco)=66, repli('0',20), padl(allt(iif(at('-',d5nume)=0, d5nume, right(d5nume,len(d5nume)-at('-',d5nume)))),20,'0'))  && R.06 Mariano

					?? w5tido
					?? padl(allt(w5nuic),20,'0')
					?? strcero(str(abs(0),15))	&& neto gravado
					?? codalicIVA(0)			&& cod. alicuota
					?? strcero(str(abs(0),15))	&& iva importe
 					?
		endif

		skip
		store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (agregue w5imp5)
	
		store '' to w5cpga,w5cpib	&& R.08 Mariano

	enddo
	
endif
set alternate to
set console on
set alternate off

** archivo de Importaciones **
s_file=strtran(s_file,'A.TXT','I.TXT')
set alternate to &s_file
set console off
set alternate on

sele f9c
if start('*'+S_tipo,'f9c')	

	do while d5sele+d5tipo='*'+S_tipo
		
	  store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (Agregue w5imp5)

	  store '' to w5cpga,w5cpib	&& R.08 Mariano
	  
      w5tido=d5tido
      w5cuit=d5cuit

      w5peri=d5peri
      w5tiop=d5tiop
	  w5coco=d5coco
	  
		w5nume	= d5nume	     
		w5fech	= d5fech   
		w5clie	= d5clie	  	  

		dime alic(21,3)

		alic=0

      do while d5sele+d5tipo='*'+S_tipo ;
         and w5clie=d5clie;
         and w5peri=d5peri;
         and w5tiop=d5tiop;
         and w5coco=d5coco;
         AND w5nume	= d5nume
         
         
			w5alic=iif(seek(d5alic,'F23'),f23.d23alic,0)
			i=val(d5alic)+1
			if !empty(d5imp1) and empty(d5imp4)	
				w5alic=0
				i=1
			endif
			if !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano
				alic(i,1)=w5alic
				alic(i,2)=alic(i,2)+d5imp4
				alic(i,3)=alic(i,3)+d5imp1
			endif	&& R.06 Mariano

			
			w5tota = w5tota + d5tota
			w5neto = w5neto + d5imp1
			w5impu = w5impu + d5imp4

			w5imp3= w5imp3 + d5imp3
			w5imp5= w5imp5 + d5imp5	&& R.06 Mariano
			w5imp6= w5imp6 + d5imp6
			w5imp7= w5imp7 + d5imp7
			w5imp8= w5imp8 + d5imp8

			&& R.08b Mariano
			if !empty(d5cpga)
				w5cpga=d5cpga
			endif
			if !empty(d5cpib)
				w5cpib=d5cpib
			endif
			&& R.08e Mariano

			if !empty(d5imp2)
				if d5imp2=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
				endif
			endif
			
			if empty(w5tiop) and !empty(d5tiop)
				w5tiop=d5tiop
			endif

         
         skip
                 
      enddo

	  skip -1 

  		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')),30)
		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f03'), f03.d3tido, '  '))

		w5nuic=iif(!d5cuit='  -        - ',left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1),'00000000000')
		
		if val(w5nuic)=0
			w5tido='99'
		endif

		for i=1 to 21
			if !empty(alic(i,1))
				w5caal=w5caal+1
			endif
		endfor

		cont_reg=1

		for i=1 to 21
*			if !empty(alic(i,1)) and !empty(d5adua) 	&& hay despacho de importacion	&& R.05 Mariano
*			if ((!empty(alic(i,1)) and w5neto!=0) or !empty(alic(i,3)))  and !empty(d5adua) 	&& hay despacho de importacion 	&& R.05 Mariano	&& R.06 Mariano
			if ( (!empty(alic(i,1)) and w5neto!=0) or (!empty(alic(i,3)) and !d5tifo$'B.C' and d5coco!='083') ) and !empty(d5adua)	&& hay despacho de importacion 	&& R.06 Mariano

					?? iif(!empty(d5adua),right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf,repl(' ',16))		&& Despacho importacion
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,3)*100),15)),15,'0')	&& neto gravado	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					?? codalicIVA(alic(i,1))			&& cod. alicuota
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,2)*100),15)),15,'0')	&& iva importe	&& R.06 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
 					?

			endif
		endfor

		if w5caal=0 and !d5tifo$'B.C' and d5coco!='083'	&& R.06 Mariano (agregue que no sea 083-tiquet sin letra)
			w5caal=1
		endif
		
*		if w5caal=1 and d5neto=0 and !empty(d5adua)	&& R.05 Mariano
		if w5caal=1 and w5neto=0 and !empty(d5adua)	&& R.05 Mariano
					?? iif(!empty(d5adua),right(dtoc(d5fech),2)+d5adua+d5dest+d5desp+d5verf,repl(' ',16))		&& Despacho importacion
					?? strcero(str(abs(0),15))	&& neto gravado
					?? codalicIVA(0)			&& cod. alicuota
					?? strcero(str(abs(0),15))	&& iva importe
 					?
		endif

		skip
		store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5impu,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.06 Mariano (agregue w5imp5)

		store '' to w5cpga,w5cpib	&& R.08 Mariano

	enddo
	
endif
set alternate to
set console on
set alternate off

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99d
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr862'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return('')


************************************************************
func percib
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpib) 	&& R.08 Mariano
if !empty(w5cpib) 	&& R.08 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prc1=1
				w_perc =w5imp6
			case f00.p0prc2=1
				w_perc =w5imp7
			case f00.p0prc3=1
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prc1=1
					w_perc =w5imp6
				case f00.p0prc2=1
					w_perc =w5imp7
				case f00.p0prc3=1
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func percga
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpga) 	&& R.08 Mariano
if !empty(w5cpga) 	&& R.08 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prc1=2
				w_perc =w5imp6
			case f00.p0prc2=2
				w_perc =w5imp7
			case f00.p0prc3=2
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prc1=2
					w_perc =w5imp6
				case f00.p0prc2=2
					w_perc =w5imp7
				case f00.p0prc3=2
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func percot
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpot)					&& R.06 Mariano
if !empty(d5cpot) or d5tico!='R'	&& R.06 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prc1=3
				w_perc =w5imp6
			case f00.p0prc2=3
				w_perc =w5imp7
			case f00.p0prc3=3
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prc1=3
					w_perc =w5imp6
				case f00.p0prc2=3
					w_perc =w5imp7
				case f00.p0prc3=3
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func otrib
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prc1=4 and !'Imp.Int'$f00.p0occ1	&& R.15 Mariano
*	case f00.p0prc1=4	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc1=3	&& R.16 Mariano
		w_perc =w5imp6
*	case f00.p0prc2=4 and !'Imp.Int'$f00.p0occ2	&& R.15 Mariano
*	case f00.p0prc2=4	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc2=3	&& R.16 Mariano
		w_perc =w5imp7
*	case f00.p0prc3=4 and !'Imp.Int'$f00.p0occ3	&& R.15 Mariano
*	case f00.p0prc3=4 	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc3=4	&& R.16 Mariano
		w_perc =w5imp8
endcase
w_perc=w_perc+w5imp5	&& R.06 Mariano

return (w_perc)

&& R.15b Mariano
************************************************************
func impmun	&& impuestos municipales
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prc1=5 	&& R.16 Mariano
	case f00.p0prc1=4 	&& R.16 Mariano
		w_perc =w5imp6
*	case f00.p0prc2=5	&& R.16 Mariano
	case f00.p0prc2=4	&& R.16 Mariano
		w_perc =w5imp7
*	case f00.p0prc3=5	&& R.16 Mariano
	case f00.p0prc3=4	&& R.16 Mariano
		w_perc =w5imp8
endcase

return (w_perc)

************************************************************
func impint
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prc1=4 and 'Imp.Int'$f00.p0occ1	&& R.15 Mariano
*	case f00.p0prc1=6 	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc1=5 	&& R.16 Mariano
		w_perc =w5imp6
*	case f00.p0prc2=4 and 'Imp.Int'$f00.p0occ2	&& R.15 Mariano
*	case f00.p0prc2=6	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc2=5	&& R.16 Mariano
		w_perc =w5imp7
*	case f00.p0prc3=4 and 'Imp.Int'$f00.p0occ3	&& R.15 Mariano
*	case f00.p0prc3=6	&& R.15 Mariano	&& R.16 Mariano
	case f00.p0prc3=5	&& R.16 Mariano
		w_perc =w5imp8
endcase

return (w_perc)

************************************************************
func codalicIVA      && Devuelve el c๓digo de alicuota IVA
************************************************************
parameters w_piva

&& R.05b Mariano
if type('w_piva')='C'	&& Codigo de alicuota
	w_piva=iif(seek(w_piva,'F23'),f23.d23alic,0)
endif
&& R.05e Mariano
do case
	case w_piva=0
		w_retu='0003'			
	case w_piva=10.50
		w_retu='0004'			
	case w_piva=21
		w_retu='0005'			
	case w_piva=27
		w_retu='0006'			
	case w_piva=5
		w_retu='0008'			
	case w_piva=2.5
		w_retu='0009'			
	otherwise
		w_retu='0000'
endcase
return (w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr862
