-- FUNCTION: public.get_colloc_as_json(character varying)

-- DROP FUNCTION public.get_colloc_as_json(character varying);

CREATE OR REPLACE FUNCTION public.get_colloc_as_json(
	tokenid character varying)
    RETURNS character varying
    LANGUAGE 'sql'
    COST 100
    VOLATILE
AS $BODY$

select (colloc_jsonb #>> '{}')::text as colloc_text
from collocations c
where c.token_id = tokenID

$BODY$;

ALTER FUNCTION public.get_colloc_as_json(character varying)
    OWNER TO postgres;
