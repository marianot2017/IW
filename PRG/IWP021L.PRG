*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP021L.PRG -> Listador de tablas
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

*) Variables-------------------------------------------------------------------

w_pfile='21l'
w_prog =upper(prompt())
w_title=ftable(w_pref,'title')
w_alta =.f.

w_dcla=space(1)
w_hcla='Z'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F21'
use (d0f21) in 0 alias 'f21' order 'd21tip'

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
do iws021l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons21l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons21l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons21l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid21l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_dcla
w_hrep=w_hcla

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f21;
	where d21tip=w_pref and between(d21cla,w_dcla,w_hcla);
	into dbf &d0f99.c order by d21tip,d21cla
use &d0f99.c alias f99c exclusive
	
if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr021l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

sele f99c
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid21l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_DCLA' or w_show
	if lastk()=p_f4
		do fhelp with 'W_DCLA',(w_pref),'iwp021','f21','d21cla','d21cla','',(p_char),'w_retu'
	endif
endif

if w_vari='W_HCLA' or w_show
	if lastk()=p_f4
		do fhelp with 'W_HCLA',(w_pref),'iwp021','f21','d21cla','d21cla','',(p_char),'w_retu'
	endif
endif

if w_dcla>w_hcla
	if !w_show
		wait wind 'Hasta debe ser mayor o igual que desde...'
	endif
	w_retu=.f.
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr021l
