-- FUNCTION: public.get_sentences_as_json(character varying)

-- DROP FUNCTION public.get_sentences_as_json(character varying);

CREATE OR REPLACE FUNCTION public.get_sentences_as_json(
	corpus character varying)
    RETURNS character varying
    LANGUAGE 'sql'
    COST 100
    VOLATILE
AS $BODY$

select (sentence_jsonb #>> '{}')::text as sentence_text
from sentences c
where c.corpus_name = corpus

$BODY$;

ALTER FUNCTION public.get_sentences_as_json(character varying)
    OWNER TO postgres;
