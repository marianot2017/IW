*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPRCIBA.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										VENTAS
*)-H Release		 : 02
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='964'
w_prog =upper(prompt())
w_title=upper('SIAP - ARCIBA - Exportar Per./Ret. Ventas I.B. Capital')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F03'
fieldvec[3]='F04'
fieldvec[4]='F05'
fieldvec[5]='F20'
fieldvec[6]='F25'

use (d0f00) in 0 alias 'f00'						&&(Parametros)
use (d0f01) in 0 alias 'f01' ORDER 'd1tip'			&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f20) in 0 alias 'f20' order 'd20tipo'		&& Condici๓n Impositiva por perํodo
use (d0f25) in 0 alias 'f25' order 'd25afip' 		&&(Percep./Retenc./Otros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

			w_campo=''
			for i=1 to 3
				w_cprv='f00.p0prv'+(str(i,1))
				if &w_cprv=1   && IB
					w_campo=w_cprv
					exit
				endif						
			endfor					
			w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
			w_campo=iif(w_campo>'0','f99b.d5imp'+w_campo,'0')

IF w_campo = "0"
			WAIT WIND "LA EMPRESA NO TIENE DEFINIDO INGRESOS BRUTOS"
			pop key
			pop menu _msysmenu
			=closall()

			&& R.01b Mariano
			dele file &d0f99.b.dbf
			dele file &d0f99.c.dbf
			DELE FILE ivf099b.idx
			&& R.01e Mariano

			RETURN
ENDIF						


sele (w_alias)

*) Variables-------------------------------------------------------------------

go bottom in f04
wdperi=f04.d4peri


*===> Es solo VENTAS

w_cpra	= .F.
w_pecp	= .F.
w_recp	= .F.

w_vta	= .T.
w_pevt	= .T.
w_revt	= .T.

w_dire	= " "
w_cuit	= ''

DO dirs

*===>									CAPITAL FEDERAL
SELE f01	
LOCATE FOR d1tip = "JU"		AND d1tdgi = "00"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			do iwsvta1.spr
ELSE
	=advgrave("NO ESTA DEFINIDA LA JURISDICCION CORRESPONDIENTE A CAPITAL FEDERAL")
ENDIF			
*)-----------------------------------------------------------------------------

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons964x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons964x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid964(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_pecp and !w_recp and !w_pevt and !w_revt
	wait window 'Debe elegir Percepciones y/o Retenciones'
	IF !w_pecp 
			_curobj = objnum(w_pecp)
	ELSE
		_curobj = objnum(w_recp)
	ENDIF
	w_retu = .F.		
	SHOW GETS
	return
endif

	=gentmp()

	if used('f99b')
		sele f99b
		use
		dele file &d0f99.b.dbf
	endif
	w_print		= .F.

	IF d0comu	= "N"
			SELE 0
			SELECT *														;		
					FROM f05												;
						WHERE d5peri = wdperi								;
							AND	 d5tipo = 'V' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0
							=advgrave("EXISTEN VENTAS FUERA DE LA JURISDICCION DE CAPITAL")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwrciba'
							w_tit1	= "ARCIBA - PERC. / RET. INGRESOS BRUTOS FUERA DE CAPITAL"
							DO listalib
							USE
			ENDIF									
	ENDIF

	sele 0
	
w_cond		= ' AND !EMPTY(d5regi) '
w_tit1	= "SIAP - ARCIBA - EXPORTACION PERC. / RET. INGRESOS BRUTOS - Capital"

IF w_pevt AND !w_revt
				w_cond	= ' AND d5regi = "P" '
				w_tit1	= "SIAP - ARCIBA - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - Capital"
ENDIF		

IF !w_pevt AND w_revt
				w_cond = ' AND d5regi = "R" '
				w_tit1	= "SIAP - ARCIBA - EXPORTACION RETENCIONES INGRESOS BRUTOS - Capital"
ENDIF		
	SELECT *													;		
		FROM f05												;
				WHERE d5peri = wdperi							;
					AND d5juri = w_prov							;
					AND d5tico <> "C"							;
					AND	 d5tipo = 'V' &w_cond.					;
				INTO CURSOR f99b

	SCATTER MEMVAR

	INDEX ON PTOI(d5peri) + d5tipo  +  DTOS(d5fech) +	;
						  + d5sucu + d5tico + d5tifo + d5nume		TO f99b
						  
	set relation to d5clie into f03
	set relation to d5clie into f02 ADDITIVE

 	go top
	if eof()
		wait window 'SIN MOVIMIENTOS PARA EXPORTAR'
		IF w_print
				do represet &&(Resetea estado)
		ENDIF
		return
	endif

	if used('f99c')
		sele f99c
		use
	endif
	dele file &d0f99.c.dbf

	DO genera

	p_report='iwrciba'
	DO listalib
	w_retu = creadir(curdir()+'\AFIP\INGBRU\CAPITAL')
	if w_retu = 0 or w_retu = 6
		COPY TO &w_dire FIELDS EXCEPT importe, monto1 TYPE SDF
	endif

IF w_print
			do represet &&(Resetea estado)
endif


return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

*===>
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
					RETURN	.F. 
	ENDIF
	IF trdate('w_fech')
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
*===>
	IF w_retu
			wkperi = PtoI(wdperi)

			if ! seek(wkperi,'f04')
					if !w_show
							wait window 'Perํodo Inexistente'
					endif
					w_retu=.f.
					_curobj	= objnum(wdperi)
					RETURN .F.
			endif
	ENDIF
	DO dirs
endif

if w_vari='W_PEVT'
	=mchkbox(@w_pevt)
endif

if w_vari='W_PEVT' or w_show
			DO dirs
endif
		
if w_vari='W_REVT'
	=mchkbox(@w_revt)
endif

if w_vari='W_REVT' or w_show
			DO dirs
			if (!w_pevt and !w_revt)
						wait window 'Debe elegir Percepciones y/o Retenciones de IB'
						IF !w_pevt 
								_curobj = objnum(w_pevt)
						ELSE
							_curobj = objnum(w_revt)
						ENDIF
						w_retu = .F.		
						SHOW GETS
			endif
endif

return (w_retu)

*===============================================================================
PROCEDURE dirs

w_prefijo	= "PR"

IF w_pevt AND !w_revt
		w_prefijo = "PV"
ENDIF		

IF !w_pevt AND w_revt
		w_prefijo = "RV"
ENDIF		

IF w_pevt OR w_revt
		w_dire=curdir()+'AFIP\INGBRU\CAPITAL\' + w_prefijo + f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire

RETURN

*================================================================================
PROCEDURE genera

	sele 0

	create dbf &d0f99.c (regimen c(1), codigo c(3), fecret C(10), tcomprob C(2), letra C(1),	; 
	nrocomprob C(15), fecomprob C(10), monto C(12), nrocertpro C(16), tipdoc C(1), nrodoc C(11),;
	sitib C(1), nroib C(10), sitiva C(1), nombre C(30), otros C(10), iva C(10), imponible C(12),;
	poralic C(5), montopract C(12), totret C(12), monto1 N(12,2), importe N(9,2))

	use &d0f99.c alias f99c
	SCATTER MEMVAR

	SELE f99b
	go top
	SCATTER MEMVAR

	do while !eof('f99b')

			SELE f99c

			IF f99b.d5regi = "R"
						m.regimen = "1"
			ELSE
				m.regimen = "2"
			ENDIF

			=SEEK("00" + f99b.d5regi + f99b.d5cpib, 'f25')
			m.codigo		= f25.d25cafi

*			m.poralic		= PADL(ALLT(STR(f25.d25porc, 5, 2)), 5, "0")
			m.poralic		= STR(f25.d25porc, 5, 2)
			m.poralic		= STRTRAN(m.poralic, ".", ",")
			
			m.fecret		= DTOC(f99b.d5fech)

			DO	tipcomprob 								&&tipo comprobante y letra

			IF f99b.d5regi	= "R"
*				m.nrocomprob = " " + f99b.d5sucu + RIGHT(f99b.d5peri, 4) + LEFT(f99b.d5nume, 6)	&& R.02 Mariano
				m.nrocomprob = f99b.d5sucu + RIGHT(f99b.d5peri, 4) + LEFT(f99b.d5nume, 6)	&& R.02 Mariano
			ELSE
*				m.nrocomprob = "   " + f99b.d5sucu + LEFT(f99b.d5nume, 8)	&& R.02 Mariano
				m.nrocomprob = "  " + f99b.d5sucu + LEFT(f99b.d5nume, 8)	&& R.02 Mariano
			ENDIF

			m.fecomprob		= DTOC(f99b.d5fech)
			
			m.monto1		= IIF(f99b.d5regi = "P", f99b.d5tota, f99b.d5imre)
			m.monto			= IIF(f99b.d5regi = "P", STR(f99b.d5tota,12,2), STR(f99b.d5imre,12,2))
			m.monto			= STRTRAN(m.monto, ".", ",")

			IF f99b.d5regi = "R"
*				m.nrocertpro = "0000" + f99b.d5sucu + LEFT(f99b.d5nume, 8)	&& R.02 Mariano
				m.nrocertpro = "000" + f99b.d5sucu + LEFT(f99b.d5nume, 8)	&& R.02 Mariano
			ELSE
				m.nrocertpro = SPACE(16)
			ENDIF

			DO sujeto

			m.otros			= STR(0,10,2)
			m.otros			= STRTRAN(m.otros, ".", ",")

			m.iva			= STR(f99b.d5imp4,10,2)
			m.iva			= STRTRAN(m.iva, ".", ",")
			
			IF f99b.d5tico = "R"
							m.imponible		= DIV( 100 * f99b.d5tota, f25.d25porc, 2) 
							m.imponible		= STR(m.imponible, 12, 2)
							m.montopract	= STR(f99b.d5tota,12,2)
							m.totret		= STR(f99b.d5tota,12,2)
							m.importe		= f99b.d5tota
			ELSE
				m.imponible		= DIV( 100 * &w_campo, f25.d25porc, 2) 
				m.imponible		= STR(m.imponible, 12, 2)
				m.montopract	= STR(&w_campo,12,2)
				m.totret		= STR(&w_campo,12,2)
				m.importe		= &w_campo
			ENDIF
			m.imponible		= STRTRAN(m.imponible, ".", ",")
			m.montopract	= STRTRAN(m.montopract, ".", ",")
			m.totret		= STRTRAN(m.totret, ".", ",")

			append blank
			gather memvar		

			SELE f99b
			SCATTER MEMVAR
			skip
	enddo

	SELE f99c
	go top

RETURN

*====================================================================================
PROCEDURE tipcomprob

m.tcomprob	= "08"
m.letra		= " "

IF f99b.d5tico = "A" OR f99b.d5tico = "8"  OR f99b.d5tico = "6"  && Factura, Recibo-Factura, Ticket-Factura
		m.tcomprob	= "01"
		m.letra		= f99b.d5tifo
ENDIF		
IF f99b.d5tico = "B"										&& Nota DB
		m.tcomprob = "02"
ENDIF		
IF f99b.d5tico = "@"										&& Recibo
		m.tcomprob = "07"
ENDIF		
IF f99b.d5tico = "R"										&& Retenciones
		m.tcomprob = "09"	
ENDIF		

RETURN

*====================================================================================
PROCEDURE sujeto

m.tipdoc	= "1"									&& DNI
m.nrodoc	= STRTRAN(SPACE(11), " ", "0")
m.sitib		= "4"									&& NO INSCRIPTO IB
m.nroib		= STRTRAN(SPACE(10), " ", "0")
m.sitiva	=  "0"
 
IF d5tico <> "R"
			IF f02.d2tido	= "80"
						m.tipdoc	= "2"			&& CUIT
			ENDIF
			IF f02.d2tido	= "86"
						m.tipdoc	= "3"			&& CUIL
			ENDIF
			IF !EMPTY(f02.d2nuib)
					IF SUBSTR(f02.d2nuib, 4, 1) = "-"
												m.sitib	= "2"
												m.nroib	= STRTRAN(f02.d2nuib, "-", "")
					ELSE
						m.sitib	= "1"
						m.nroib	= "0" + STRTRAN(f02.d2nuib, "-", "0")		&& Local
					ENDIF
			ENDIF
			m.nrodoc	= STRTRAN(f02.d2cuit, "-", "")
			m.nombre	= f02.d2nomb
			m.sitiva	= FTIIMP(f02.d2clie, f99b.d5peri, 'C')
ELSE
	IF f03.d3tido = "80"
				m.tipdoc	= "2"			&& CUIT
	ENDIF
	IF f03.d3tido = "86"
				m.tipdoc	= "3"			&& CUIL
	ENDIF
	IF !EMPTY(f03.d3nuib)
			IF SUBSTR(f03.d3nuib, 4, 1) = "-"									&& Convenio
										m.sitib	= "2"
										m.nroib	= STRTRAN(f03.d3nuib, "-", "")
			ELSE										
				m.sitib	= "1"
				m.nroib	= "0" + STRTRAN(f03.d3nuib, "-", "0")
			ENDIF
	ENDIF
	m.nrodoc	= STRTRAN(f03.d3cuit, "-", "")
	m.nombre	= f03.d3nomb
	m.sitiva	= FTIIMP(f02.d2clie, f99b.d5peri, 'C')
ENDIF

m.nrodoc	= PADL(m.nrodoc, 11, "0")
IF m.sitiva $ "1.2.3.4.5.6.7"
			IF m.sitiva $ "3.7"				
						m.sitiva = "5"
			ENDIF
			IF m.sitiva	= "4"
						m.sitiva = "3"
			ENDIF						
			IF m.sitiva	= "6"
						m.sitiva = "4"
			ENDIF						
ELSE
	m.sitiva	=  "0"
ENDIF

RETURN

*================================================================================

***puesto adrede para que incluya el report al proyecto**
report form iwrciba
