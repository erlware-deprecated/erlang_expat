-record(xml_start, {ns = <<"">>,
		    name = <<"">>,
		    attrs = []}).

-record(xml_end, {ns = <<"">>,
		  name = <<"">>}).

-record(xml_char_data, {char_data = <<"">>}).
