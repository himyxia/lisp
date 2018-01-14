set-signal! on wire
	↓
call wire's action-procedures one by one
	↓
install time_procedure on agenda
	↓
propagate call each item on agenda(then remove them)
