-record(question,
    {
        text = none,
        answers = [],
        str_callrec = none
    }).

-record(answer,
    {
        text = none,
        when_chosen = none
    }).

-record(callrec_stack_item,
	{
		origin_callrec = none,
		reached_rec_val = none,
		context = [],
		spawned = [],
    	sent =[],
    	received = [],
    	consumed = [],
    	% children_nodes = [],
    	result = none,
        trace = none
	}).	

-record(call_info,
	{
		call = none,
		pos_pp = none
	}).

-record(receive_info,
	{
		pos_pp = none,
		context = [],
		bindings = [],
		clause = none
	}).