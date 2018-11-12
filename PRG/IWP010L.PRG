*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP010L.PRG -> Listador de empresas
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

w_pfile='10l'
w_alias='f10'
w_title=db_name(w_alias)

w_code=space(2)
w_coha='99'
wdnomb=space(10)
whnomb='zzzzzzzzzz'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F10'
use (d0f10) in 0 alias 'f10' order 'p0coem' &&(Empresas)

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
do iws010l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons10l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons10l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons10l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid10l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_code
w_hrep=w_coha

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f10;
	where between(p0coem,w_code,w_coha) and between(p0empr,wdnomb,whnomb);
	into dbf &d0f99.c order by p0coem
use &d0f99.c alias f99c exclusive
	
if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr010l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid10l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_CODE' or w_show
	if lastk()=p_f4
		do fhelp with 'W_CODE','','iwp010','f10','p0coem','p0coem','',(p_strc),'w_retu'
	else
		w_code=strcero(w_code)
		show get w_code
	
	endif
endif

if w_vari='W_COHA' or w_show
	if lastk()=p_f4
		do fhelp with 'W_COHA','','iwp010','f10','p0coem','p0coem','',(p_strc),'w_retu'
	else
		w_coha=strcero(w_coha)
		show get w_coha
	endif
	if w_code>w_coha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WHNOMB' .or. w_show
	if empty(whnomb)
		whnomb='ZZZZZZZZZZ'
	endif
	show get whnomb
	if alltrim(wdnomb)>alltrim(whnomb)
		wait 'Hasta debe ser mayor o igual a desde' wind nowai
		return .f.
	endif
endif

return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr010l
