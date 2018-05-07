-- FUNCTION: public.get_corpus_as_json(character varying)

-- DROP FUNCTION public.get_corpus_as_json(character varying);

CREATE OR REPLACE FUNCTION public.get_corpus_as_json(
	corpus character varying)
    RETURNS character varying
    LANGUAGE 'sql'
    COST 100
    STABLE
AS $BODY$

select (wfl_jsonb #>> '{}')::text as wfl_text
from corpora c
where c.corpus_name = corpus

$BODY$;

ALTER FUNCTION public.get_corpus_as_json(character varying)
    OWNER TO postgres;
