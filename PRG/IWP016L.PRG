*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP016L.PRG -> Listador de Definicion de informes
*)-H
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

w_pfile='16l'
w_title=db_name('f17')

w_dede='000'
w_deha='999'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F16'
fieldvec[2]='F17'

use (d0f16) in 0 alias 'f16' order 'd16defi' &&(Definicion de inf. C)
use (d0f17) in 0 alias 'f17' order 'd17defi' &&(Definicion de inf. R)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws016l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons16l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons16l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons16l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid16l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_dede
w_hrep=w_deha

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f16;
	where between(d16defi,w_dede,w_deha);
	into dbf &d0f99.c order by d16defi
use &d0f99.c alias f99c exclusive
	
if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
set relation to d16defi into f17
set skip to f17
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr016l'
	=rep_form()
	p_report=''
else &&(to file)
	p_repfields='d16defi,d16nomb,d16ley1,d16ley2,f17.d17orde,f17.d17titu,f17.d17impr,f17.d17filt,f17.d17form,f17.d17come'
endif

do represet &&(Resetea estado)

sele f99c
set skip to
set relation to
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid16l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_DEDE' or w_show
	w_dede=strcero(w_dede)
	show get w_dede
	if lastk()=p_f4
		do fhelp with 'W_DEDE','','iwp016','f16','d16defi','d16defi','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_DEHA' or w_show
	w_deha=strcero(w_deha)
	show get w_deha
	if lastk()=p_f4
		do fhelp with 'W_DEHA','','iwp016','f16','d16defi','d16defi','',(p_strc),'w_retu'
	endif
	if w_dede>w_deha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr016l
