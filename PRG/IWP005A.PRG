*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005A.PRG -> Generacion de Asientos
*	 RELEASE 		 : 12
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_tipo

PRIVATE ALL LIKE w_*, w*

if !lockemp(nf_coem)
	=errornet(050)
	read when .f.
	return
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear      &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.

*) Datos del proceso-----------------------------------------------------------

w_pfile='05a'
w_alias='f05'
w_title='GENERACION DE ASIENTOS'+iif(s_tipo='V',' VENTAS',' COMPRAS')
w_imre	= "N"

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'

use (d0f00) in 0 alias 'f00'					  EXCL &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  EXCL &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie' EXCL &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' EXCL &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' EXCL &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri' EXCL &&(Comprobantes)

*)-----------------------------------------------------------------------------

sele f04
go bott
wperi =f04->d4peri
go top

wdfech={}
whfech=date()

wdsucu = "     "	&& R.12 Mariano (Agrande 1 digito)

w_marca	= .F.				&& R.03 Ra๚l

IF s_tipo = "V"
		   wdsucu=strcero(wdsucu) 
ELSE
	wdsucu = "     "	&& R.12 Mariano (Agrande 1 digito)
ENDIF

whsucu='99999'	&& R.12 Mariano (Agrande 1 digito)

wejer=iif(val(right(w0fipre,2))<80,'20','19')+right(w0fipre,2)

&& R.06b Ra๚l

wmarca		= 0
wxdfech		= {}
wejer2		= wejer						
wejer1		= iif(val(right(w0fipre,2))<80,'20','19') + STRTRAN(STR(VAL(right(w0fipre,2))-1,2,0),' ','0')		
wfipre		= subst(w0fipre,1,len(w0fipre)-2) + STRTRAN(STR(VAL(right(w0fipre,2))-1,2,0),' ','0')		

&& R.06e Ra๚l

w4subd='0'

wfech =ctod('01/'+wperi)+31
wfech =ctod('01/'+right(dtoc(wfech),7))-1
whfech=wfech

wdeta =f00->p0deta

do ssm998.mpr &&(Menu Ayuda)

whfech1	= wfech

do iws005a.spr

pop key
pop menu _msysmenu
=unlockemp(nf_coem)
=closall()
return

*=============================================================================
&& R.06b Ra๚l
PROC when05a

w_retu=.t.
w_vari=varread()
p_curobj=_curobj

*IF w_vari='WPERI' or w_show
		=valejer()
*ENDIF

&& R.06e Ra๚l

		
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid05a
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

&& R.09b Mariano
***vfp***
if version()='Visual Fox' and (w_pfile!='05a' or wontop()!='IWS005A' or wmarca=3)
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***
&& R.09e Mariano

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	w_fech	= CTOD('01/' + wperi)
	IF EMPTY(w_fech)
				RETURN	.F. 
	ENDIF
	IF TRDATE('w_fech')
			wperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wperi
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
				w_retu	= .F.
				_curobj	= objnum(wperi)
				RETURN .F.
		ENDIF
	ENDIF
	if !seek(ptoi(wperi),'f04')
		if !w_show
			wait window 'Perํodo Inexistente.'
		endif
		w_retu=.f.
	else
		if (f04.d4esve<>'C' and s_tipo='V') or (f04.d4esco<>'C' and s_tipo='C')
			if !w_show
				wait wind 'Perํodo de IVA Abierto.'
			endif
			w_retu=.f.
		else
			sele f05
			if !seek(ptoi(wperi)+s_tipo,'f05')
				if !w_show
					wait wind 'Perํodo de IVA sin movimientos.'
				endif
				w_retu=.f.
			endif
			if ((!empty(f04.d4asve) and s_tipo='V') or (!empty(f04.d4asco) and s_tipo='C')) and w_retu
				if sino('Este Perํodo de IVA ya ha sido Imputado. Imputa Nuevamente ?.')='N'
					w_retu=.f.
				endif
			endif
		endif
*===>
		if w_vari='WPERI' and w_retu
				&& R.08b Mariano
				if s_tipo='V'
					wdfech=CTOD('01/' + wperi)
					show get wdfech
					if d0cciva = 'S'	
						wxdfech=wdfech
					endif
				endif
				&& R.08e Mariano
				wfech	= CTOD('01/' + wperi) + 31
				wfech	= ctod('01/'+right(dtoc(wfech),7)) - 1
				whfech	= wfech
				whfech1	= wfech
				IF d0cciva = 'S'
							wejer = '2000'
				ELSE							
*					wejer	= RIGHT(wperi,4)		&& R.06 Ra๚l
					=valejer()						&& R.01 Ra๚l
				ENDIF
				show get wfech
				show get whfech
				SHOW GET wejer
		endif
	endif

&& R.06b Ra๚l
&& R.03b Ra๚l
*	IF w_vari='WPERI' AND w_retu
*				DO verpeparti
*				SHOW GET wfech
*				SHOW GET whfech
*				SHOW GET wejer
*	ENDIF
&& R.03e Ra๚l
&& R.06e Ra๚l
endif

if w_vari='WDFECH' or w_show
		IF !EMPTY(wdfech)
			IF TRDATE('wdfech')
							SHOW GET wdfech
			ELSE
				IF !w_show
						WAIT WIND "Fecha incorrecta"
				ENDIF
				w_retu	= .F.
				_curobj	= objnum(wdfech)
				RETURN	.F.
			ENDIF
		endif	&& R.08 Mariano
&& R.06b Ra๚l
*			IF DTOS(wdfech) < DTOS(wxdfech)		AND !EMPTY(wxdfech)		AND s_tipo='C'	&& R.08 Mariano
			IF DTOS(wdfech) < DTOS(wxdfech)		AND !EMPTY(wxdfech)		AND s_tipo='V'	&& R.08 Mariano
					IF !w_show
							WAIT WIND "Fecha previa a perํodo de asientos"
					ENDIF
					w_retu	= .F.
					_curobj	= objnum(wdfech)
					RETURN	.F.
			ENDIF
&& R.06e Ra๚l

*		ENDIF	&& R.08 Mariano
endif

if w_vari='WHFECH' or w_show
		IF TRDATE('whfech')
				SHOW GET whfech
		ELSE
			IF !w_show
					WAIT WIND "Fecha incorrecta"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(whfech)
			RETURN .F.
		ENDIF

		if wdfech>whfech
			if !w_show
				wait wind 'Hasta debe ser mayor a desde.'
			endif
			w_retu=.f.
		endif
		IF whfech > whfech1
			if !w_show
				wait wind 'Fecha hasta POSTERIOR al fin del perํodo.'
			endif
			w_retu=.f.
		ENDIF

		wfech	= whfech					&& R.06 Ra๚l
		SHOW GET wfech						&& R.06 Ra๚l
endif

if w_vari='WDSUCU' or w_show
			IF s_tipo = "V"
				wdsucu=strcero(wdsucu) 
			ELSE
				wdsucu = "     "	&& R.12 Mariano (Agrande 1 digito)
			ENDIF
			show get wdsucu
endif

if w_vari='WHSUCU' or w_show
   whsucu=strcero(whsucu) 
	show get whsucu
	if wdsucu>whsucu
		if !w_show
			wait wind 'Hasta debe ser mayor a desde.'
		endif
		w_retu=.f.
	endif
endif

*if w_vari='WEJER' 	&& R.04 Mariano
if w_vari='WEJER' or w_show 	&& R.04 Mariano
	if empty(wejer)
		if !w_show
			wait wind 'Debe especificar el a๑o del ejercicio.'
		endif
		w_retu=.f.
	else	&& R.04b Mariano
		IF d0cciva != 'S'	&& R.05 Mariano
			if !valejer()
				w_retu=.f.
			endif	&& R.04e Mariano
		endif	&& R.05 Mariano
	endif
endif

*if w_vari='W4SUBD' or w_show
*	do fhelp with 'W4SUBD','','cgp004','f04c','d4tipo','d4desc','Subdiario inexistente.',(p_strc),'w_retu'
*endif

if w_vari='WFECH' or w_show
		IF !TRDATE('wfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(wfech)
			RETURN .F.
		ELSE
			IF wperi <> RIGHT(DTOC(wfech),7)
						IF !w_show
								WAIT WIND "La fecha no corresponde al perํodo"
						ENDIF
						w_retu	= .F.
						_curobj	= objnum(wfech)
						RETURN .F.
			ENDIF
		ENDIF
endif

&& R.06b Ra๚l

*IF iif(type('f00c.p0asha')='U', between(DTOS(whfech), DTOS(f00c.p0ejde), DTOS(f00c.p0ejha)) , between(DTOS(whfech), DTOS(f00c.p0asde), DTOS(f00c.p0asha)))			&& R.07 Ra๚l
IF (wmarca = 2) AND iif(type('f00c.p0asha')='U', between(DTOS(whfech), DTOS(f00c.p0ejde), DTOS(f00c.p0ejha)) , between(DTOS(whfech), DTOS(f00c.p0asde), DTOS(f00c.p0asha)))
		IF wejer = wejer1
				wejer = wejer2
		ENDIF
ENDIF		

*IF iif(type('f00c1.p0asha')='U', between(DTOS(whfech), DTOS(f00c1.p0ejde), DTOS(f00c1.p0ejha)) , between(DTOS(whfech), DTOS(f00c1.p0asde), DTOS(f00c1.p0asha)))			&& R.07 Ra๚l
IF USED('f00c1')	&& R.11 Mariano
	IF (wmarca = 1) AND iif(type('f00c1.p0asha')='U', between(DTOS(whfech), DTOS(f00c1.p0ejde), DTOS(f00c1.p0ejha)) , between(DTOS(whfech), DTOS(f00c1.p0asde), DTOS(f00c1.p0asha)))
		IF wejer = wejer2
				wejer = wejer1
		ENDIF
	ENDIF		
ENDIF	&& R.11 Mariano

SHOW GET wejer

&& R.06e Ra๚l

					
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05a
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05a0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05a0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid05a(.t.)
	wait 'Debe completar todos los datos!.' wind
	return
endif

&& R.06b Ra๚l
IF used('f00c1')
		SELE f00c1
		USE
ENDIF
						
IF used('f00c')
		SELE f00c
		USE
ENDIF

wmarca=3	&& R.08 Mariano

_curobj = objnum(wperi)				&& R.06 Ra๚l

&& R.06e Ra๚l

wfi88=.t.

*) Apertura de archivos de CONTA-----------------------------------------------

wfipre=subst(w0fipre,1,len(w0fipre)-2)+right(wejer,2)

*if !mfile(wfipre+'00.dbf')
*	=advsuave('Ejercicio Contable Inexistente')
*	return
*endif

ncf00=wfipre+'00'
ncf01=wfipre+'01'
ncf02=wfipre+'02'
ncf03=wfipre+'03'
ncf04=wfipre+'04'
ncf20=left(wfipre,len(wfipre)-8)+'cof020'
ncf18=wfipre+'18'
ncf19=wfipre+'19'

w_acce	= 1

do while .t.

	fieldvec[1]='F00C'
	fieldvec[2]='F01C'
	fieldvec[3]='F02C'
	fieldvec[4]='F03C'
	fieldvec[5]='F04C'
	fieldvec[6]='F20C'

	use (ncf00) in 0 alias 'f00c' EXCL
	use (ncf01) in 0 alias 'f01c' EXCL order 'd1cuen' 	&&(Cuentas)
	use (ncf02) in 0 alias 'f02c' EXCL order 'd2tipo' 	&&(H.asientos)
	use (ncf03) in 0 alias 'f03c' EXCL order 'd3tipo' 	&&(D.asientos)
	use (ncf04) in 0 alias 'f04c' EXCL order 'd4tipo' 	&&(Subdiarios)
	use (ncf20) in 0 alias 'f20c' 						&&(Auditoria de Conta)

	if file(ncf18+'.dbf') and file(ncf19+'.dbf')		&& version conta con cto.costo
		fieldvec[7]='F18C'
		fieldvec[8]='F19C'

		use (ncf18) in 0 alias 'f18c' EXCL order d18codi  && Centro de costos
		use (ncf19) in 0 alias 'f19c' EXCL order d19cuen  && asigna ctro.ctos a ctas.
	endif

	if !neterr()
		exit
	endif

	fieldvec=''
	=CieCo05a()
	w_acce	= w_acce + 1
	IF w_acce > 3
				pop key
				pop menu _msysmenu
				=unlockemp(nf_coem)
				=closall()
				RETURN
	ENDIF
enddo

if f00c->p0esej='C'
	=advsuave('Ejercicio Contable Cerrado...','Informaci๓n')
	=CieCo05a()
	return
endif

*if (wfech < F00C->p0ejde .or. wfech > F00C->p0ejha .or. Empty(wfech)) and !('CO'$d0paco and d0emcg=='99')				&& R.03 Ra๚l
*if (wfech < F00C->p0asde .or. wfech > F00C->p0asha .or. Empty(wfech)) and !('CO'$d0paco and d0emcg=='99')				&& R.03 Ra๚l	&& R.04 Mariano
if (Empty(wfech) or iif(type('f00c.p0asde')='U', !between(wfech,f00c.p0ejde,f00c.p0ejha), !between(wfech,f00c.p0asde,f00c.p0asha)) ) and !('CO'$d0paco and d0emcg=='99')	&& R.04 Mariano
	*) Hago la validacion de fecha Aqui para que no interfiera con
	*) Con la generacion
	=advgrave('Fecha de Asiento Fuera de Perํodo Contable.')
	=cieco05a()
	return
endif

if sino('Confirma Generaci๓n de Asiento a Verificar ?.','N')='N'
	=cieco05a()
	return
endif

*) Validacion de estado de subdiario

if seek(w4subd,'f04c')
	if subs(f04c->d4cier,month(wfech),1)='C'
		wait wind 'Subdiario Cerrado en '+mes(wfech) timeout 1
		=cieco05a()
		return
	endif
else
	wait wind 'Subdiario Contable Inexistente' timeout 1
	=cieco05a()
	return
endif

*) Fin Validacion de estado de subdiario
*) Creacion de archivo temporario

sele f02c
*)=ciect05a()

copy stru to &d0f99

w_acce = 1
DO WHILE .T.
			use (d0f99) exclu in 0 alias 'f99'
			IF !NETERR()
					EXIT
			ENDIF
			w_acce = w_acce + 1
			IF w_acce > 3
						pop key
						pop menu _msysmenu
						=unlockemp(nf_coem)
						=closall()
						RETURN
	ENDIF
ENDDO

sele f99

index on d2cuen tag cuenta
go top in f99

wait wind 'Generando Asiento a Verificar...' nowait

w2asie=f04c->d4asie
w2asie=subs(str(1000001+val(w2asie),7),2,6)

if f00c->p0esej='A' .and. w2asie='000001' .and. w4subd='0'
	w2asie=subs(str(1000001+val(w2asie),7),2,6)
endif
w2fech=wfech
w2reng='0000'
wdetaf=iif(wdeta='S',.t.,.f.)

=start(ptoi(wperi)+s_tipo,'f05')
wbreak=.f.


do while (f05->d5peri+f05->d5tipo)=(wperi+s_tipo)   ;
		.and. !eof('f05')

	if inkey()=p_esc
		wbreak = .t.
		exit
	endif

	IF w_imre = "N"		AND f05.d5tico = "R"
				SKIP IN f05
				LOOP
	ENDIF

	if empty(f05->d5tota)
		skip in f05
		loop
	endif
	if f05->d5fech < wdfech .or. f05->d5fech > whfech
		skip in f05
		loop
	endif
	if s_tipo='V'
		if f05->d5sucu < wdsucu .or. f05->d5sucu > whsucu
			skip in f05
			loop
		endif
	endif

	w_newstatmsg='Perํodo:'+' '+f05->d5peri+' '+iif(f05->d5tipo='V','Ventas','Compras')+' '+;
		'Comprobante:'+' '+tcabre(f05->d5tico)+' '+f05->d5nume
	set mess to w_newstatmsg

	*) Pases de Ventas / Compras
	w_cuen	= ""
	IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
				if !seek(f05->d5clie,'f02')
									wait wind 'Cliente Inexistente.'+f05->d5clie + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
									wbreak=.t.
				ENDIF
	ELSE
		if ! seek(f05->d5clie,'f03')
						wait wind 'Proveedor Inexistente '+f05->d5clie + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
						wbreak=.t.
		ENDIF
	ENDIF
	do case
			*) Cta.Corriente
			case f05->d5cond='1'
						IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
									w_cuen=f02->d2cuen
						ELSE
							w_cuen=f03->d3cuen
						ENDIF

			*) Contado
			case f05->d5cond='2'
						IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
									w_cuen=f02->d2cuec
						ELSE
							w_cuen=f03->d3cuec
						ENDIF

			* Factura de Credito
			case f05->d5cond='3'
						IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
									w_cuen=f02->d2cuef
						ELSE
							w_cuen=f03->d3cuef
						ENDIF
	endcase
	if !seek(w_cuen,'f01c')
				WAIT WIND 'Cuenta '+trim(w_cuen)+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
				wbreak=.t.
	else
		if f01c->d1hijo<>0
				WAIT WIND 'Cuenta '+trim(w_cuen)+' NO Imputable.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
					wbreak=.t.
		endif
	endif

*=========================================================================
*	IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
*				if !seek('RV'+f05->d5rubr,'f01')
*								wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
*								wbreak=.t.
*				ENDIF
*	ELSE
*		if !seek('RC'+f05->d5rubr,'f01')
*			wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
*			wbreak=.t.
*		ENDIF
*	ENDIF

	IF (s_tipo = "V"  AND f05.d5tico <> "R") 
				if !seek('RV'+f05->d5rubr,'f01')
								wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
								wbreak=.t.
				ENDIF

	ENDIF
	IF (s_tipo = "C"  AND f05.d5tico <> "R") 
				if !seek('RC'+f05->d5rubr,'f01')
								wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
								wbreak=.t.
				ENDIF

	ENDIF
	IF (s_tipo = "V"  AND f05.d5tico = "R") 
				if !seek('RP'+f05->d5rubr,'f01')
								wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
								wbreak=.t.
				ENDIF

	ENDIF
	IF (s_tipo = "C"  AND f05.d5tico = "R") 
				if !seek('RS'+f05->d5rubr,'f01')
								wait wind 'Rubro '+f05->d5rubr+' Inexistente.' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
								wbreak=.t.
				ENDIF

	ENDIF



*	Validacion Existencia de cuentas parametricas
	if !empty(f05->d5imp1)
				wbreak = wbreak .or. vacu(f01->d1cimp1)
	endif
	if !empty(f05->d5imp2)
				wbreak = wbreak .or. vacu(f01->d1cimp2)
	endif
	if !empty(f05->d5imp3)
				wbreak = wbreak .or. vacu(f01->d1cimp3)
	endif
	if !empty(f05->d5imp4)
				wbreak = wbreak .or. vacu(f01->d1cimp4)
	endif
	if !empty(f05->d5imp5)
				wbreak = wbreak .or. vacu(f01->d1cimp5)
	endif
	if !empty(f05->d5imp6)
				wbreak = wbreak .or. vacu(f01->d1cimp6)
	endif
	if !empty(f05->d5imp7)
				wbreak = wbreak .or. vacu(f01->d1cimp7)
	endif
	if !empty(f05->d5imp8)
				wbreak = wbreak .or. vacu(f01->d1cimp8)
	endif

*	Leyenda del asiento
	if !empty(f05->d5leye)
						w2apre=f05->d5leye
	else
		IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
					w2apre= 'C '+f05->d5clie+' '+tcabre(f05->d5tico)+' '+(f05->d5tifo+f05->d5sucu+'-'+f05->d5nume)

		ELSE
			w2apre= 'P '+f05->d5clie+' '+tcabre(f05->d5tico)+' '+(f05->d5tifo+f05->d5sucu+'-'+f05->d5nume)
		ENDIF
	endif

*	IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
	IF s_tipo = "V" 
				w_deha	= "H"
	ELSE		
		w_deha	= "D"
	ENDIF		
	
	DO pases

*===> Pase CLI / PROV
	IF w_deha	= "H"
				=GRAB05a((w_cuen), 'd5tota', "D", wdetaf)
	ELSE
		=GRAB05a((w_cuen), 'd5tota', "H", wdetaf)
	ENDIF
		
	skip in f05
enddo

wait clear

if empty(reccount('f99'))
	wait wind 'No existen Comprobantes del '+dtoc(wdfech)+' al '+dtoc(whfech) timeout 1
	=ciect05a()
	return
endif

sele f99
set orde to

*) para borrar todo lo que da cero
dele for empty(d2debe+d2habe)
pack

wtdebe = 0
wthabe = 0

w2reng='0000'
go bott in f99
w_rec=reccount('f99')
go top in f99

do while recno('f99')<=w_rec

	wtdebe = wtdebe + f99->d2debe
	wthabe = wthabe + f99->d2habe

	*) recorro el 99 hasta el ultimo registro que existia antes
	*) de iniciar este proceso. Esto es porque en el mismo se dan de alta nuevos.

	if empty(f99->d2debe)
		*) si el debe es cero quiere decir que solo hubo haber
		*) por lo tanto queda este como unico pase de haber
		w2reng=subs(str(10001+val(w2reng),5),2,4)
		repl f99->d2reng with w2reng
	else
		*) si el debe no es cero quiere decir que pudo existir tambiem haber
		if empty(f99->d2habe)
			*) si el haber fue cero queda este como unico pase de debe
			w2reng=subs(str(10001+val(w2reng),5),2,4)
			repl f99->d2reng with w2reng
		else
			*) si el haber es distinto de cero debo dar de alta otro pase
			*) para tenerlo en cuenta
			w_2cuen=f99->d2cuen   && guardo cuenta
			w_2habe=f99->d2habe   && guardo importe de haber
			w_2apre=f99->d2apre   && guardo comentario

			w2reng=subs(str(10001+val(w2reng),5),2,4)

			repla f99->d2reng with w2reng  &&(le coloco nro de renglon)
			repla f99->d2habe with 0     	 &&(blanqueo haber)

			w_wrec=recno('f99')
			=net_appe('f99')
			=set_alia('f99')
			repl f99->d2tipo with w4subd,;
				f99->d2asie with w2asie,;
				f99->d2fech with w2fech,;
				f99->d2cuen with w_2cuen,;
				f99->d2apre with w_2apre,;
				f99->d2habe with w_2habe,;
				f99->d2debe with 0
			w2reng=subs(str(10001+val(w2reng),5),2,4)
			repl f99->d2reng with w2reng
			goto w_wrec in f99
			=rest_alia()
		endif
	endif

	skip in f99
enddo

go top in f99
w_reng='0000'
do while !eof('f99') and type('f99.d2cocc')='C'

	if val(f99.d2cocc)=0	&& cuando viene vacio o con 000 hago el tratamiento

 		sele f19c
		=seek(f99.d2cuen,'f19c')
    	sum d19pocc to w_topo while f99.d2cuen=d19cuen
			
		sele f99
				
	   if w_topo=100

			scatter memvar
	    	=seek(f99.d2cuen,'f19c')

			** grabo primer asignacion **
       	w_reng=strcero(str(val(w_reng)+1,4))
       	m.d2reng=w_reng
			replace d2reng with m.d2reng, d2cuen with m.d2cuen, d2cocc with f19c.d19cocc, d2debe with round(m.d2debe*f19c.d19pocc/100,2), d2habe with round(m.d2habe*f19c.d19pocc/100,2)
			skip in f19c
			
			** grabo siguientes asignaciones **
	    	do while f99.d2cuen=f19c.d19cuen and !eof('f19c')
	    	
        	    w_reng=strcero(str(val(w_reng)+1,4))
        	    m.d2reng=w_reng
		
    	       insert blank
				
				 replace d2tipo with m.d2tipo, d2asie with m.d2asie, d2fech with m.d2fech, ;
							d2reng with m.d2reng, d2cuen with m.d2cuen, d2cocc with f19c.d19cocc, ;
							d2debe with round(m.d2debe*f19c.d19pocc/100,2), d2habe with round(m.d2habe*f19c.d19pocc/100,2), ;
							d2apre with m.d2apre
						
				 skip in f19c
			enddo

		else
     	   w_reng=strcero(str(val(w_reng)+1,4))
   	   m.d2reng=w_reng
	    	=seek(f99.d2cuen,'f19c')
			if f99.d2cuen=f19c.d19cuen	&& elijo el primero de los asignados
	   		replace d2reng with m.d2reng, d2cocc with f19c.d19cocc
	   	else
	   	   replace d2reng with m.d2reng, d2cocc with '000'
	   	endif
		endif
	else
      w_reng=strcero(str(val(w_reng)+1,4))
      m.d2reng=w_reng
  	  	replace d2reng with m.d2reng
   endif
   skip in f99
enddo

set inde to
inde on d2reng tag reng

wait wind f04c->d4desc+spac(04)+str(wtdebe,14,2)+spac(01)+str(wthabe,14,2) timeout 1

go top in f99

if eof('f99')
	wait wind 'No Se Gener๓ Ning๚n Movimiento' timeout 1
	=ciect05a()
	return
endif

=skipbar('Registro','Confirmar',.t.)
=skipbar('Registro','Elegir',.t.)

IF wbreak	OR str(wtdebe,19,2)<>str(wthabe,19,2)
	IF wbreak
			wait wind 'Hubo alg๚n ERROR en el proceso.'
	ELSE
		wait wind 'Asiento NO balancea.'
	ENDIF
	&& R.09b Mariano
    ***vfp***
*	do browasie
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'browasie()'
    else
   		do browasie
    endif
    ***vfp***
    && R.09e Mariano
	if sino('Imprime error por impresora ?.')='S'
		w_same='S'
		w_nupm='S'
		w_tota='S'
		w_come='S'
		w_howimp=.t.
		do impas with w_howimp
	endif
	=ciect05a()

	=skipbar('Registro','Confirmar',.f.)
	=skipbar('Registro','Elegir',.f.)

	return
endif

&& R.09b Mariano
***vfp***
*do browasie
if version()='Visual FoxPro'
	*** cambio el lastkey() ***
	keyboard '{tab}'
	=inkey()
	DO wbrow WITH 'browasie()'
else
	do browasie
endif
***vfp***
&& R.09e Mariano

if sino('Imprime asiento por impresora ?.')='S'
	=skipbar('Registro','Confirmar',.t.)
	=skipbar('Registro','Elegir',.t.)
	w_same='S'
	w_nupm='S'
	w_tota='S'
	w_come='S'
	w_howimp=.t.
	do impas with w_howimp
	=skipbar('Registro','Confirmar',.f.)
	=skipbar('Registro','Elegir',.f.)
endif

if 'CO'$d0paco and d0emcg=='99'
	=ciect05a()
	return
endif

s_sino=2
if sino(' Incorpora el Asiento '+w2asie+' a la Contabilidad ? ')='S'

*	if wfech < f00c->p0ejde .or. wfech > f00c->p0ejha .or. empty(wfech)		&& R.03 Ra๚l
*	if wfech < f00c->p0asde .or. wfech > f00c->p0asha .or. empty(wfech)		&& R.03 Ra๚l	&& R.04 Mariano
	if empty(wfech) or iif(type('f00c.p0asde')='U', !between(wfech,f00c.p0ejde,f00c.p0ejha), !between(wfech,f00c.p0asde,f00c.p0asha))		&& R.04 Mariano
		*) Hago la validacion de fecha Aqui para que no interfiera con
		*) la generacion
		wait wind 'Fecha de Asiento Fuera de Perํodo Contable' timeout 1

	else

		wait wind 'Incorporando Asiento: '+w2asie nowait

		*) Incorporacion de temporario y actualizacion de cuentas
		=set_alia('f03c')
		=net_appe('f03c')
		repl f03c->d3tipo with w4subd,;
			f03c->d3asie with w2asie,;
			f03c->d3memo with ''

		w3refe=strcero(str(val(f04c->d4refe)+1,5))
		repl f03c->d3refe with w3refe

		=rest_alia()

		wact=(w4subd=='0')
		wmes=strcero(str(month(w2fech),2))
		whmes='f01c->d1ha'+wmes
		wdmes='f01c->d1de'+wmes

		go top in f99
		do while wact .and. !(eof('f99'))
			if seek(f99->d2cuen,'f01c')
				repl &whmes with f99->d2habe+&whmes;
					,&wdmes with f99->d2debe+&wdmes
			else
				wait wind 'ERROR. CONTABILIDAD MAL ACTUALIZADA' timeout 1
			endif
			skip in f99
		enddo

		sele f99 &&(Es obligatorio para poder hacer el appe from)
		use

		sele f02c
		appe from &d0f99

		*) esta rutina setea el vector de cierres de subdiarios al dar de alta
		*) el primer movimiento de ese mes con una P de pendiente (cierre)
		sele f04c

		if subs(f04c->d4cier,val(wmes),1)=' '
			repl f04c->d4cier with stuff(f04c->d4cier,val(wmes),1,'P')
		endif

		* Actualizaciขn de ฃltimo numero asiento.

		repl f04c->d4asie with w2asie
		* Actualizaciขn de ๚ltimo numero de referencia.
		repl f04c->d4refe with w3refe

		=set_alia('f20c')
		=net_appe('f20c')
		repl  f20c.d20empr with d0coem       ,;
				f20c.d20ejer with wejer        ,;
				f20c.d20fech with date()       ,;
				f20c.d20time with time()       ,;
				f20c.d20tiop with 'IV'+s_tipo   ,;
				f20c.d20leha with wperi        ,;
				f20c.d20lede with w4subd+'R'+w3refe+' A'+w2asie
		=rest_alia()

		* Si hubo Actualizacion de saldos: EStado de EJercicio
		if wact
			if f00c->p0esej='P'
				wait wind 'Actualizando Estado de Ejercicio Contable' timeout .4
				repl f00c->p0esej with 'Q'
			endif
		endif

		* Grabo Asiento en Perกodos
		if s_tipo='V'
			repl f04->d4asve with wejer+' '+w4subd+' '+w2asie+' '+dtoc(wfech)
		else
			repl f04->d4asco with wejer+' '+w4subd+' '+w2asie+' '+dtoc(wfech)
		endif

		=audi(iif(s_tipo='V','IVV','IVC'),'A'+w2asie+' R'+w3refe,wperi)

		wait clear
	endif

	sele f00
	repl f00->p0deta with wdeta

else
	wait wind 'Asiento Descartado' timeout 1
endif

=ciect05a()

=skipbar('Registro','Confirmar',.f.)
=skipbar('Registro','Elegir',.f.)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC grab05A
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para w2cuen,g_impo,g_tipo,g_deta

wimpo=f05->&g_impo * iif(g_tipo='D',1,-1)  && aqui le cambio el signo

stor 0 to w2debe,w2habe
if wimpo>0
	w2debe=wimpo
else
	w2habe=-wimpo
endif

if ! seek(w2cuen,'f99') .or. g_deta
	wapre=''
	if g_deta
		wapre=w2apre
	else
		if s_tipo='V'
			wapre='Ventas '
		else
			wapre='Compras '
		endif
		if wdeta='N'
			wapre=wapre + dtoc(wdfech) +'-'+ dtoc(whfech)
		else
			wapre=wapre+'de '+mes(w2fech)
		endif
	endif
	=set_alia('F99')
	=net_appe('f99')
	repl f99->d2tipo with w4subd,;
		f99->d2asie with w2asie,;
		f99->d2fech with w2fech,;
		f99->d2cuen with w2cuen,;
		f99->d2apre with wapre
	=rest_alia()
endif

repl f99->d2debe with f99->d2debe+w2debe,;
	f99->d2habe with f99->d2habe+w2habe

return ''

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC cieco05a &&(Cierra archivos de CONTA)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
IF USED('f00c')
		sele f00c
		use
ENDIF
IF USED('f01c')
		sele f01c
		use
ENDIF
IF USED('f02c')
		sele f02c
		use
ENDIF
IF USED('f03c')
		sele f03c
		use
ENDIF
IF USED('f04c')
		sele f04c
		use
ENDIF
IF USED('f20c')
		sele f20c
		use
ENDIF
if used('f18c')
	use in f18c
endif
if used('f19c')
	use in f19c
endif
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC ciect05a  && cierra archivos de IVA y borra transitorio
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
=cieco05a()

if !used('f99')
	dele file &d0f99..dbf
	dele file &d0f99..cdx
else
	sele f99
	use
	dele file &d0f99..dbf
	dele file &d0f99..cdx
endif
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC impas
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter w_howimp
sele f99
go top

do repdest

if w_howimp
	p_repclaus='noconsole to print prompt'
else
	p_repclaus='preview prompt'
endif

d0apre='S'
if !empty(p_repclaus)
	p_report='iwr005a'
	=rep_form()
	p_report=''
else
	p_repwhile='p_cancela'
endif

do represet
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC vacu &&(Valida cuentas)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para v_cuen
priv v_erro
v_erro=.f.
if seek(v_cuen,'F01c')
	if f01c->d1hijo<>0
		v_erro=.t.
		wait wind ctop(v_cuen)+'  Cuenta No Imputable' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
	endif
else
	v_erro=.t.
	wait wind v_cuen+'  Cuenta Inexistente' + CHR(13) + CHR(13) + "Comprobante : " + f05.d5leye
endif

return v_erro

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC browasie
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

set mess to 'Debe: '+str(wtdebe,14,2)+space(10)+'Haber: '+str(wthabe,14,2)

if !wexist('browasie')
	define wind browasie;
		at 0,0 size srows(),scols();
		noclose nominimize nofloat title 'Asiento  : '+d2asie+space(10)+'<Esc> para salir' ;
		font 'arial',10
endif

sele f99
set rela to d2cuen into f01c

if type('f99.d2cocc')='C'
	brow fields;
		d2reng :h='Reng' ,;
		d2fech :h='Fecha',;
		cuen=ctop(d2cuen) :h='Cuenta',;
		d2cocc :h='CC',;
		d2apre :h='Aplic/Refer.' :30,;
		d2debe :h='Debe' ,;
		d2habe :h='Haber',;
		desc=iif(!empty(f01c.d1desc),f01c.d1desc,'Cuenta inexistente') :h='Descripci๓n';
		nomenu;
		noappe;
		noedit;
		nodele;
		wind browasie
else
	brow fields;
		d2reng :h='Reng' ,;
		d2fech :h='Fecha',;
		cuen=ctop(d2cuen) :h='Cuenta',;
		d2apre :h='Aplic/Refer.' :30,;
		d2debe :h='Debe' ,;
		d2habe :h='Haber',;
		desc=iif(!empty(f01c.d1desc),f01c.d1desc,'Cuenta inexistente') :h='Descripci๓n';
		nomenu;
		noappe;
		noedit;
		nodele;
		wind browasie
endif

sele f99
set rela to

if wexist('browasie')
	rele wind browasie
endif

set mess to csalida

&& R.09b Mariano
***vfp***
IF version()='Visual FoxPro'
	clear read
endif
***vfp***
&& R.09e Mariano
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC versummary
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
p_summary=.t.
return('')

*=============================================================================
PROCEDURE pases

		*) Pase correspondiente al importe 1  NETO GRAVADO
		=grab05a((f01->d1cimp1),'d5imp1',w_deha,wdetaf)

		*) Pase correspondiente al importe 2  NO GRAVADO O EXEN
		=grab05a((f01->d1cimp2),'d5imp2',w_deha,wdetaf)

		*) Pase correspondiente al importe 4  IVA
		=grab05a((f01->d1cimp4),'d5imp4',w_deha,wdetaf)

		*) Pase correspondiente al importe 5  IVA ADICIONAL
		=grab05a((f01->d1cimp5),'d5imp5',w_deha,wdetaf)

		*) Pase correspondiente al importe 3  IVA Pe/Re/Otros
		=grab05a((f01->d1cimp3),'d5imp3',w_deha,wdetaf)

		*) Pase correspondiente al importe 6  OTROS CONCEPTOS 1
		=grab05a((f01->d1cimp6),'d5imp6',w_deha,wdetaf)

		*) Pase correspondiente al importe 7  OTROS CONCEPTOS 2
		=grab05a((f01->d1cimp7),'d5imp7',w_deha,wdetaf)

		*) Pase correspondiente al importe 8  OTROS CONCEPTOS 3
		=grab05a((f01->d1cimp8),'d5imp8',w_deha,wdetaf)

		*) Pase correspondiente a RETENCIONES IB y GAN y OTROS
		IF f05.d5tico = "R"
					w_retu = .F.
					DO buscaret
					IF w_retu
							=grab05a((&pc_imp),'d5tota',w_deha,wdetaf)
					ENDIF
		ENDIF


RETURN

*=============================================================================
PROCEDURE buscaret

*f00.p0pr{c/v}[n]							n = 1, 2, 3
*					= 1 va en {c/v}im6
*					= 2 va en {c/v}im7
*					= 3 va en {c/v}im8					

w_retu	= .F.
pn_n	= 4								&&		= 4/6/7/8		4 = SIN ASIGNAR

pc_imp	= "f01.d1cimp"
	
IF (s_tipo = "V"  AND f05.d5tico <> "R") OR (s_tipo = "C" AND f05.d5tico = "R")
				pc_pr	= "f00.p0prv"
ELSE
	pc_pr	= "f00.p0prc"
ENDIF

DO CASE
		CASE !EMPTY(f05.d5pere)		&& IVA
						pn_n = 3
		CASE !EMPTY(f05.d5cpib)		&& Ingresos Brutos
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 1
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
		CASE !EMPTY(f05.d5cpga)		&& Ganancias
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 2
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
		CASE !EMPTY(f05.d5cpot)		&& Otros 
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 3
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
ENDCASE

IF pn_n <> 4
		pc_imp	= pc_imp + STR(pn_n,1)
		w_retu	= .T.
ENDIF

RETURN w_retu

&& R.06b Ra๚l
*============================================================================================
FUNCTION valejer
*============================================================================================

wmarca 	= 0
wtipo	= 0

IF d0cciva = 'S'	
			RETURN
ENDIF

DO previo

DO ultimo

wfech	= whfech
wfech1	= wfech

*IF wmarca = 0									&& R.07 Ra๚l
IF wmarca = 0	AND ((f04.d4esve='C' and s_tipo='V') or (f04.d4esco='C' and s_tipo='C'))		&& R.07 Ra๚l
*	=advsuave('Ejercicio Contable Inexistente')	&& R.10 Mariano
	=advsuave('Ejercicio Contable Cerrado o Inexistente')	&& R.10 Mariano
	wejer	= ''
	SHOW GET wejer
	RETURN
endif

SHOW GET wperi
show get wfech
SHOW GET wdfech
show get whfech
SHOW GET wfech1

RETURN

*==========================================================================
PROCEDURE ultimo

IF !USED('f00c')
	IF mfile(w0fipre+'00.dbf')
				use (w0fipre+'00.dbf') in 0 alias 'f00c' EXCL
	ELSE
		RETURN
	ENDIF		
ENDIF
IF iif(type('f00c.p0asha')='U', between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0ejde),7)), PTOI(RIGHT(DTOC(f00c.p0ejha),7)) ), between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0asde),7)), PTOI(RIGHT(DTOC(f00c.p0asha),7)) ) ) 
					wejer	= wejer2												
					wmarca	= 2
*					IF type('f00c.p0asha')<>'U'	AND s_tipo = 'C'	&& R.08 Mariano 
					IF type('f00c.p0asha')<>'U'		&& R.08 Mariano
						if s_tipo = 'V'	&& R.08 Mariano 
							IF PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0ejde),7))
												wdfech	= f00c.p0asde
												wxdfech	= wdfech
*							ELSE	&& R.08b Mariano
*								wdfech = {}
							ENDIF
							IF USED('f00c1')	&& R.11 Mariano
								&& R.10b Mariano
								if between(ptoi(wperi),PTOI(RIGHT(DTOC(f00c1.p0ejde),7)),PTOI(RIGHT(DTOC(f00c1.p0ejha),7)))
									wxdfech	= wdfech
								endif	
								&& R.10e Mariano
							endif	&& R.11 Mariano
						else
							wxdfech	= wdfech
						endif	&& R.08e Mariano
					ENDIF
ENDIF
RETURN

*==========================================================================
PROCEDURE previo

wfipre = subst(w0fipre,1,len(w0fipre)-2) + STRTRAN(STR(VAL(right(w0fipre,2))-1,2,0),' ','0')		

IF !USED('f00c1')
	IF mfile(wfipre+'00.dbf')
				use (wfipre+'00.dbf') in 0 alias 'f00c1' EXCL
	ELSE
		RETURN
	ENDIF		
ENDIF
IF iif(type('f00c1.p0asha')='U', between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c1.p0ejde),7)), PTOI(RIGHT(DTOC(f00c1.p0ejha),7)) ), between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c1.p0asde),7)), PTOI(RIGHT(DTOC(f00c1.p0asha),7)) ) )  	
					wejer	= wejer1												
					wmarca	= 1
*					IF type('f00c1.p0asha')<>'U'	AND s_tipo = 'C' 	&& R.08 Mariano
					IF type('f00c1.p0asha')<>'U'	&& R.08 Mariano
						if s_tipo = 'V' 	&& R.08 Mariano
							IF PTOI(wperi) == PTOI(RIGHT(DTOC(f00c1.p0ejde),7))
												wdfech	= f00c1.p0asde
												wxdfech	= wdfech
*							ELSE	&& R.08b Mariano
*								wdfech = {}
							ENDIF
							&& R.10b Mariano
							if between(ptoi(wperi),PTOI(RIGHT(DTOC(f00c1.p0ejde),7)),PTOI(RIGHT(DTOC(f00c1.p0ejha),7)))
								wxdfech	= wdfech
							endif	
							&& R.10e Mariano
						else
							wxdfech	= wdfech
						endif	&& R.08e Mariano
					ENDIF
ENDIF

RETURN

*==========================================================================
&& R.06e Ra๚l




*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
report form iwr005a


*===> Los pas้ al final para que quede el c๓digo viejo y hacerla de nuevo.

FUNCTION xvalejer

&& R.03b Ra๚l
*wfipre=subst(w0fipre,1,len(w0fipre)-2)+right(wperi,2)

IF mfile(w0fipre+'00.dbf')
		use (w0fipre+'00.dbf') in 0 alias 'f00c' EXCL
*		IF PTOI(RIGHT(DTOC(f00c.p0asde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0asha),7))		&& R.03 Ra๚l	&& R.04 Mariano
		IF iif(type('f00c.p0asde')='U', between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0ejde),7)), PTOI(RIGHT(DTOC(f00c.p0ejha),7)) ), between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0asde),7)), PTOI(RIGHT(DTOC(f00c.p0asha),7)) ) )  	&& R.04 Mariano
						wfipre	= w0fipre
						wejer	= wejer2									&& R.06 Ra๚l			
		ELSE
*			wfipre	= subst(w0fipre,1,len(w0fipre)-2)+right(wperi,2)		&& R.06 Ra๚l
			wejer	= wejer1 
		ENDIF
		SELE f00c
		USE
ENDIF		
&& R.03e Ra๚l


&& R.06b Ra๚l
&& R.02b Ra๚l
if !mfile(wfipre+'00.dbf')
	=advsuave('Ejercicio Contable Inexistente')
	wejer	= ''
	RETURN
endif
IF d0cciva = 'N'	&& R.06 Ra๚l
			RETURN
ENDIF
&& R.06e Ra๚l

*use (wfipre+'00.dbf') in 0 alias 'f00c' EXCL

*IF PTOI(RIGHT(DTOC(f00c.p0ejde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0ejha),7))
*				wejer	= RIGHT(DTOC(f00c.p0ejha),4)
*				sele f00c
*				use
*				RETURN
*ENDIF

*sele f00c
*use

&& R.06b Ra๚l
*IF mfile(wfipre+'00.dbf')
*	use (wfipre+'00.dbf') in 0 alias 'f00c' EXCL

**	IF PTOI(RIGHT(DTOC(f00c.p0ejde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0ejha),7))		&& R.03 Ra๚l
**	IF PTOI(RIGHT(DTOC(f00c.p0asde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0asha),7))		&& R.03 Ra๚l	&& R.04 Mariano
*	IF iif(type('f00c.p0asde')='U', between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0ejde),7)), PTOI(RIGHT(DTOC(f00c.p0ejha),7)) ), between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0asde),7)), PTOI(RIGHT(DTOC(f00c.p0asha),7)) ) )  	&& R.04 Mariano

*					wejer	= RIGHT(DTOC(f00c.p0ejha),4)
*					sele f00c
*					use
*					RETURN
*	ENDIF
*	sele f00c
*	use
*ENDIF
&& R.06e Ra๚l

&& R.02e Ra๚l

*wfipre=subst(w0fipre,1,len(w0fipre)-2) +	RIGHT(STR(VAL(RIGHT(wperi,4)+1),4), 2)
*wfipre=subst(w0fipre,1,len(w0fipre)-2) +	RIGHT( STR( VAL(wejer) + 1, 4, 0), 2) && R.05 Mariano

IF d0cciva = 'S'	&& R.05 Mariano
	wfipre=w0fipre	&& R.05 Mariano
else				&& R.05 Mariano
	if !mfile(wfipre+'00.dbf')
		=advsuave('Ejercicio Contable Inexistente')
		wejer	= ''
*		RETURN 	&& R.04 Mariano
		return .f.	&& R.04 Mariano
	endif
endif	&& R.05 Mariano

use (wfipre+'00.dbf') in 0 alias 'f00c' EXCL

*IF PTOI(RIGHT(DTOC(f00c.p0ejde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0ejha),7))		&& R.03 Ra๚l
*IF PTOI(RIGHT(DTOC(f00c.p0asde),7)) <= PTOI(wperi)		AND PTOI(wperi) <= PTOI(RIGHT(DTOC(f00c.p0asha),7))		&& R.03 Ra๚l	&& R.04 Mariano
IF iif(type('f00c.p0asde')='U', between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0ejde),7)), PTOI(RIGHT(DTOC(f00c.p0ejha),7)) ), between( PTOI(wperi), PTOI(RIGHT(DTOC(f00c.p0asde),7)), PTOI(RIGHT(DTOC(f00c.p0asha),7)) ) )  	&& R.04 Mariano
				wejer	= RIGHT(DTOC(f00c.p0ejha),4)
				sele f00c
				use
				RETURN
ENDIF

sele f00c
use

RETURN

*======================================================================================
PROCEDURE	xverpeparti

&& R.03b Ra๚l
w_marca = .F.

IF mfile(w0fipre+'00.dbf')
		use (w0fipre+'00.dbf') in 0 alias 'f00c' EXCL
*		IF PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0asha),7))		&& R.03 Ra๚l	&& R.04 Mariano
		IF iif(type('f00c.p0asha')='U', PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0ejha),7)), PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0asha),7)) )		&& R.04 Mariano
*						whfech	= f00c.p0asha	&& R.04 Mariano
						whfech	= iif(type('f00c.p0asha')='U', f00c.p0ejha, f00c.p0asha)	&& R.04 Mariano
						whfech1	= whfech
						wfech	= whfech
						wfech1	= wfech
						w_marca = .T.
		ENDIF
		SELE f00c
		USE
ENDIF		

&& R.06b Ra๚l
wfipre	= subst(w0fipre,1,len(w0fipre)-2) + STRTRAN(STR(VAL(right(w0fipre,2))-1,2,0),' ','0')		&& R.06 Ra๚l
IF mfile(wfipre+'00.dbf')
		use (wfipre+'00.dbf') in 0 alias 'f00c' EXCL
		IF iif(type('f00c.p0asha')='U', PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0ejha),7)), PTOI(wperi) == PTOI(RIGHT(DTOC(f00c.p0asha),7)) )		
						whfech	= iif(type('f00c.p0asha')='U', f00c.p0ejha, f00c.p0asha)	
						whfech1	= whfech
						wfech	= whfech
						wfech1	= wfech
						w_marca = .T.
		ENDIF
		SELE f00c
		USE
ENDIF		

&& R.06e Ra๚l

RETURN

&& R.03e Ra๚l

if .f.
*if w_vari='WEJER' 	&& R.04 Mariano
if w_vari='WEJER' or w_show 	&& R.04 Mariano
	if empty(wejer)
		if !w_show
			wait wind 'Debe especificar el a๑o del ejercicio.'
		endif
		w_retu=.f.

&& R.06b Ra๚l
*	else	&& R.04b Mariano
*		IF d0cciva != 'S'	&& R.05 Mariano
*			if !valejer()
*				w_retu=.f.
*			endif	&& R.04e Mariano
*		endif	&& R.05 Mariano
&& R.06e Ra๚l

	endif
endif
endif
