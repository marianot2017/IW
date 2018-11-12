*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP004L.PRG -> Listador de Periodos
*)-H Release		 : 01
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

w_prog=upper(prompt())
w_alta=.f.

*) Variables del report--------------------------------------------------------

w_pfile='04l'
w_alias='f04'
w_title=db_name(w_alias)

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F04'
fieldvec[3]='F05'
fieldvec[4]='F13'

use (d0f00) in 0 alias 'f00'						     &&(Tablas)
use (d0f04) in 0 alias 'f04' order 'd4peri' exclu &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'		  &&(Comprobantes)
use (d0f13) in 0 alias 'f13' order 'd13peri'		  &&(Anticipos y retenciones)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

sele f04
go top
w_pede=d4peri
go bott
w_peha=d4peri

do ssm998.mpr
do iws004l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons04l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid04l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_pede
w_hrep=w_peha

if used('f99c')
	sele f99c
	zap
else
	sele 0
	create dbf &d0f99.c (d4peri c(7),d4esve c(1),d4esco c(1))
	use &d0f99.c alias f99c exclusive
endif

selec f04
=seek(ptoi(w_pede),'f04')
do while !eof('f04') and ptoi(f04.d4peri)<=ptoi(w_peha)
	insert into f99c values (f04.d4peri,f04.d4esve,f04.d4esco)
	skip in f04
enddo

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr004l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid04l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_PEDE' or w_show
*	w_pede=strcero(left(w_pede,2))+'/'+strcero(right(w_pede,4))	&& R.01 Mariano
*	show get w_pede	&& R.01 Mariano
	if lastkey()=p_f4
		do fhelp with 'W_PEDE','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif

	&& R.01b Mariano
	w_fech	= CTOD('01/' + w_pede)
	IF EMPTY(w_fech)
		RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
		w_pede	= RIGHT(DTOC(w_fech),7)
		SHOW GET w_pede
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(w_pede)
		RETURN .F.
	ENDIF
	&& R.01e Mariano

   wkperi=PtoI(w_pede)
   if !seek(wkperi,'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif
endif

if w_vari='W_PEHA' or w_show
*	w_peha=strcero(left(w_peha,2))+'/'+strcero(right(w_peha,4))	&& R.01 Mariano
*	show get w_peha	&& R.01 Mariano
	if lastkey()=p_f4
		do fhelp with 'W_PEHA','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif

	&& R.01b Mariano
	w_fech	= CTOD('01/' + w_peha)
	IF EMPTY(w_fech)
		RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
		w_peha	= RIGHT(DTOC(w_fech),7)
		SHOW GET w_peha
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(w_peha)
		RETURN .F.
	ENDIF
	&& R.01e Mariano

   wkperi=PtoI(w_peha)

*===>
	SELE f04

   if !seek(wkperi,'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif
endif

if ptoi(w_pede)>ptoi(w_peha)
	if !w_show
		wait wind 'Hasta debe ser mayor o igual que desde...'
	endif
	w_retu=.f.
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr004l
