*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP010e.PRG -> Eleccion de empresas
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Release		 : 04
*)-H
*)-H  Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w0emp1=''
d0mene=.f.

***vfp***
if version()='Visual FoxPro'
*	on key label f1 do nada
	on key label f2 do nada
	on key label f3 do nada
	on key label f4 do nada
	on key label f5 do nada
	on key label f6 do nada
	on key label f7 do nada
	on key label f8 do nada
	on key label f9 do nada
*	on key label f10 do nada
	on key label f11 do nada
	on key label f12 do nada
endif
***vfp***

do prot10e
f_acce=1
do while .t.
	fieldvec[1]='f10'
	fieldvec[2]='f66'

	use (d0f10) in 0 alias f10 order p0coem  &&(Empresas)
	use (d0f66) in 0 alias f66 order d66user &&(Usuarios por empresas)	
	if .not. neterr()
		exit
	else
		if f_acce>=3	&& tres intentos
			=logout()
			=mquit()
  		endif
		f_acce=f_acce+1
	endif

	wait 'Espere un momento...' wind nowait
enddo
fieldvec=''

sele f10
d0mene=.t.

d0f99=sys(2023)+'\IVF'+netuser+'99'
sele f10

if wsuper

	*) Esto quedo de cuando el supervisor elejia empresa
	*) se supone que a partir de ahora no lo hara mas
	copy to (d0f99) with cdx
	
else
	copy to (d0f99) struct with cdx
	use &d0f99 in 0 alias f99 exclu

	go top in f66
	do while .not. eof('f66')
		if f66.d66user=ps_user
			if seek(f66.d66empr,'f10')
				sele f10
				scatter memvar
				sele f99
				appe blan
				gather memvar
			else
				wait 'La empresa '+f66.d66empr+' asignada al usuario '+ps_user+'fue eliminada !!.' wind
			endif
		endif
		skip in f66
	enddo	
	sele f99
	use

endif

sele f10
use
sele f66
use

use &d0f99 in 0 alias f10 order p0coem
sele f10

if eof('f10')
	wait ' Usuario '+alltr(ps_user)+' Sin empresas asignadas !!! ' wind
	sele f10
	use
	=logout()
	=closall()
	dele file &d0f99..dbf
	dele file &d0f99..cdx	
	clear
	set sysmenu to default
	cancel
endif

if winflag
	&&(Esta entrando al sistema)
	d0del=1	
else
	&&(Esta Cambiando de Empresa)
	w0path=d0path
	w0coem=d0coem
	w0emp =d0emp
	ww0nivel=w0nivel
	w0vein=d0vein
	w0vees=d0vees
	w0veda=d0veda
endif


do while .t.

	&&(Si No es la primera vez que entra Se posiciona en la empresa)
	
	if nf_rean<>nf_reem .and. !winflag
		nf_reem=nf_rean
		nf_logi='N'
		nf_stat=' '

		=login('E',.t.)
	endif

	go top
	keyboard chr(0)

	d0coem=alltrim(p0coem)
	d0path=alltrim(p0path)
	d0emp =p0empr
	d0vein=p0vein
	d0vees=p0vees
	d0veda=p0veda
	nf_coem=d0coem

	w_empr=1
	w_ejse=1
	wesq='C'+d0coem+'??00.dbf'

	if !wsuper
		sele f10
		set skip of menu _msysmenu .t.

*===> Para que se posicione en la empresa actual
		SEEK pp0coem
		w_empr = RECNO()
*===>

		do iws010e.spr

*===> Salva la empresa elegida
		pp0coem = p0coem
*===>
		set skip of menu _msysmenu .f.
	else
		=when10e()
		w0emp1='Configuraci๓n de Seguridad'
	endif
	
	if lastkey()=p_esc

		if winflag
			if sino(' Sale del Sistema ? ','No')='S'
				=logout()
				=mquit()
			endif
			loop
		endif

		d0coem=w0coem
		d0emp= w0emp
		d0path=w0path
		w0nivel=ww0nivel
		d0vein=w0vein
		d0vees=w0vees
		d0veda=w0veda
		nf_coem=d0coem
		d0nede=.5
		d0mene=.t.
		nf_reem=0
		nf_logi='N'
		nf_stat=' '		
		=login('E')
		=closall()
		return
		
	else
	
		d0coem=alltrim(p0coem)
		d0path=alltrim(p0path)
		d0emp =p0empr
		d0vein=p0vein
		d0vees=p0vees
		d0veda=p0veda
		nf_coem=d0coem
		
	endif
	
	if empty(d0coem)

		wait 'Error: No ha elegido Empresa' wind nowait
		loop

	else
		wcont=0
		w0nivel='1'

		*) Deja pendiente la apertura de la empresa anterior
		*)--------------------------------------------------
		
		if !winflag
			nf_rean=nf_reem
			nf_logi=' '
			nf_stat='E'		
			=login('E',.t.)
		endif
		
		*) Chequeo que la version no este desactualizada
		*)----------------------------------------------
		if f10->p0vees+f10->p0veda <> wversexe+wversexe
			if f10.p0vees+f10.p0veda='999999'

				w_err1='Empresa tomada de DOS con problemas en Archivos'
				w_err2='Ver reporte de Auditorํa'
				
				=advgrave(w_err1+chr(13)+w_err2)
				
			else
				=errornet(003)
			endif
			sele f10
			loop
		endif
		
		d0mene=.f.
		d0nede=.1
		
		ww_esca=.f.
		nf_puab=.f.
		nf_reem=0
		nf_logi=' '
		nf_stat='E'
		
		if !login('E')
		
			d0mene=.t.
			d0nede=.5
			sele f10
		
			if nf_exit &&(Se detecto que hay mas de 1 netuser con el mismo numero)
				=logout()
				=mquit()
			endif
			loop
		
		else
		
			* Chequeo que no exista ninguna terminaciขn anormal
			* -------------------------------------------------
			nf_stre=.t.
			nf_reba=.t.
			if terano('R')
				d0mene=.f.
				if .not. rlock('f10')
					=errornet(052)
					sele f10
					d0mene=.t.
					d0nede=.5
					=logout()
					loop
				endif
				set skip of menu _msysmenu .t.
				do teranor.spr
				set skip of menu _msysmenu .f.

				nf_stre=.f.

				if !nf_stre
					d0mene=.t.					
					unlock in f10
					sele f10
					d0mene=.t.
					d0nede=.5
					=logout()
					loop
				endif

			else

				if !nf_reba
					=errornet(053)
					d0mene=.t.					
					unlock in f10
					sele f10
					d0mene=.t.
					d0nede=.5
					=logout()
					loop
				else
					nf_reba=.f.
				endif

			endif
		endif
		
		if nf_puab
			=cleartran(nf_coem)
			nf_puab=.f.
		endif

		if !winflag &&(En Este momento deja la empresa Anterior.)
			ww_reem=nf_reem
			nf_reem=nf_rean
			=logout()
			nf_reem=ww_reem
			nf_rean=ww_reem
		else
			nf_rean=nf_reem
		endif
		
		nf_logi='N'
		nf_stat=' '
		
		=login('E',.t.)
		
		d0mene=.t.
		d0nede=.5
		unlock in f10

		if ww_esca
			return
		endif

	endif
	
	exit

enddo

d0f00='\iw\'+d0coem+'\'+'ivf000'

f_acce=1
do while .t.
	fieldvec[1]='f00'
	use (d0f00) in 0 alias f00 &&(Parametrs generales)
	if .not. neterr()
		exit
	else
		if f_acce>=3	&& tres intentos
			=logout()
			=mquit()
   		endif
		f_acce=f_acce+1
	endif
enddo

fieldvec=''

sele f00
IF EMPTY(p0emp)
		=db_rlock('f00')
		REPLA p0emp		WITH d0emp
		unlock
ENDIF

d0f00='\iw\'+d0coem+'\'+trim(p0f00)
d0f01=d0path+trim(p0f01)
d0f02=d0path+trim(p0f02)
d0f03=d0path+trim(p0f03)
d0f04=d0path+trim(p0f04)
d0f05=d0path+trim(p0f05)
d0f06=d0path+trim(p0f06)
d0f07=d0path+trim(p0f07)
d0f08=d0path+trim(p0f08)
d0f09=d0path+trim(p0f09)
d0f11=d0path+trim(p0f11)
d0f13=d0path+trim(p0f13)
d0f14=d0path+trim(p0f14)
d0f15=d0path+trim(p0f15)
d0f20=d0path+trim(p0f20)

PUBLIC d0imp,d010c,d017c,d0exp,d0len
d0imp=p0imp
d010c=trim(p010c)
d010c=&d010c
d017c=trim(p017c) &&(Impresion)
d017c=&d017c
d0exp=trim(p0exp)
d0exp=&d0exp
d0len=trim(p0len)
d0len=&d0len
d0del=p0del &&(Retardo)

*) Variables de modo de trabajo

PUBLIC d0emcg,d0emco,d0coim,d0maac,d0poce
*)-----------------------------------------------------------------------------
*)PUBLIC d0imfa,d0comu
*) Se declaran en el 000.
*)-----------------------------------------------------------------------------


PUBLIC d0cciva, d0juce, d0afip, d0gan, d0ibvta, d0ibcpra, d0ganvta, d0gancpra

d0ibvta		= .F.
d0ibcpra	= .F.
d0ganvta	= .F.
d0gancpra	= .F.
d0cciva	=	p0cciva					&& Usa Contabilidad IVA
d0juce	=	p0juce					&&(Jurisdicciขn)
d0afip	=	p0afip					&& (Jurisdicci๓n AFIP)
d0gan	=	p0gan					&& (Ganancias PF o Soc.)


d0emcg=p0emcg &&(imputa contabilidad)

d0emco=p0emco &&(emite comprobantes)
d0imfa=p0imfa &&(emite comprobantes)
d0coim=p0coim &&(condicion impositiva)
d0macc=p0macc &&(maneja cuenta corriente)
d0comu=p0comu &&(Convenio Multilateral 'S')
d0poce=p0poce &&(Porcentaje central)

*) Picture de importes

PUBLIC d0piim,d0pisa,d0inte
d0piim=alltrim(p0piim)
d0pisa=alltrim(p0pisa)
d0Inte=strcero(p0Inte)

****use

if used('f10')
	sele f10
	use
endif
dele file &d0f99..dbf
dele file &d0f99..cdx

use (d0f12) in 0 alias 'f12'
sele f12

PUBLIC d0ivan,d0ivaa,d0paco,d0deci,d0deno,d0ptcv,d0PiVe,d0stcv,d0PiCo,d0peto,d0peta
*d0ivan=p0ivan
*d0ivaa=p0ivaa
d0peto=p0peto
d0peta=p0peta
d0paco=trim(p0paco)
d0deci=p0deci
d0deno=p0deno
d0PiCo='!!!!!!!!!!!!!!'	&& R.04 Mariano (agregue un ! mas)
d0ptcv=8
*d0stcv=4	&& R.03 Mariano
d0stcv=8	&& R.03 Mariano
d0PiVe='!!!!!!!!!!!!!'	&& R.03 Mariano
d0PiVe='!!!!!!!!!!!!!!!!!'	&& R.03 Mariano

use

PUBLIC d0vant, d0peri
*
use (d0f10) in 0 alias f10 order p0coem  &&(Empresas)
SELE f10
IF SEEK(d0coem, 'f10')
		=db_rlock('f10')
		IF p0aviso = 'S'
					w_retu	= .T.
					w_mensa	= ''
					DO ortiva	WITH ' '
					IF !w_retu
							=advgrave(w_mensa)
							w_resul	=	sino("Desea seguir recibiendo este tipo de recordatorio ?","SI", "Parแmetros indefinidos")
							REPLA p0aviso	WITH w_resul
					ENDIF
		ENDIF
		d0peri	= p0peri
		d0vant	= p0vant
		UNLOCK
ENDIF 
USE

IF USED('f00')
	SELE f00
	USE
ENDIF

if '\IW\'$sys(2023) or len(sys(2023))=3
	d0f99=d0path+'I'+d0coem+netuser+'99'
else
	d0f99=sys(2023)+'\I'+d0coem+netuser+'99'
endif

&& R.01b Ra๚l
*if !mfile(alltrim(d0f01+'.cdx'))
if mfile('indexar.mem') or mfile(d0coem+'\indexar.mem') or !mfile(d0f01+'.cdx')		
   do db_indexiw with .f.
   dele file indexar.mem
   dele file &d0coem.\indexar.mem
   dele file indexar.mem
	=closall()
endif
&& R.01e Ra๚l


* VARIABLES CONTABLES
* ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿

Public d0pict,d0picp,d0niv1,d0niv2,d0niv3,d0niv4,d0niv5,d0niv6
public w0fipre &&(file prefijo seteado por inico() c:\co\c0091)


=inico() &&((Inicializaci๓n de variables publicas para la conta)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when10e
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private ncad1,ncad2,cfill

w_temp=''
d0coem=alltrim(p0coem)
d0path=alltrim(d0path)
wesq='C'+d0coem+'??00.dbf'
ncad1= len(d0coem)
d0emp =p0Empr
d0VeIn=p0VeIn
d0VeEs=p0VeEs
d0VeDa=p0VeDa
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid10e
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_vari=varread()
w_retu=.t.

if w_vari='W_EMPR'

	if w_retu
		if lastkey()=p_enter
			w_title=alltrim(p0empr)
			d0audi=p0audi
			clear read
*			w_title=w_title+space(95-(len(w_title)+len(w_sys)))+w_sys
			w_title=w_title+space(5)+w_sys+' MEMOSOFTฎ'	&& R.02 Mariano (agregue marca memosoft)
			if _windows
				modi wind screen;
					title w_title ;
					fill file memofond.bmp icon file iw.ico
				zoom wind screen max
			endif
		endif
	endif

endif
return(w_retu)

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure prot10e
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if used('s02')
	sele s02
	use
endif
return

*========================================================================================
