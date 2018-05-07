-- FUNCTION: public.get_conc_as_json(character varying, character varying)

-- DROP FUNCTION public.get_conc_as_json(character varying, character varying);

CREATE OR REPLACE FUNCTION public.get_conc_as_json(
	corpusname character varying,
	tokenid character varying)
    RETURNS character varying
    LANGUAGE 'sql'
    COST 100
    VOLATILE
AS $BODY$

select (conc_jsonb #>> '{}')::text as conc_text
from concretisations c
where c.token_id = tokenID
and c.corpus_name = corpusName

$BODY$;

ALTER FUNCTION public.get_conc_as_json(character varying, character varying)
    OWNER TO postgres;
