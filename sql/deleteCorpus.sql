

delete from collocations
where 	corpus_id in (
	select col.corpus_id from collocations col
inner join corpora cor
on col.corpus_id = cor.corpus_id
where corpus_name = 'necessary'
);

delete from concretisations
where corpus_id in (
	select con.corpus_id from concretisations con
inner join corpora cor
on con.corpus_id = cor.corpus_id
where corpus_name = 'necessary'
);


delete from corpora
where corpus_name = 'necessary';

select corpus_name from corpora;
