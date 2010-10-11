from lxml import html

class Failure(Exception):
    pass

def name_getter(question):
    def _list_of(url):
        parser = html.parse(url)
        return [el.text for el in parser.xpath('//td/table/tr/td[1]')
                if el.text]
    url_db = dict(
        male_names='http://names.mongabay.com/male_names.htm',
        female_names='http://names.mongabay.com/female_names.htm',
        surnames='http://names.mongabay.com/most_common_surnames.htm'
    )
    try:
        url = url_db[question]
    except KeyError:
        raise Failure
    return _list_of(url)

class KnowledgeMeta(type):
    # (question, fetcher)
    answers = dict(
        male_names=name_getter,
        female_names=name_getter,
        surnames=name_getter
    )

    def __getattr__(self, key):
        try:
            getter = self.answers[key]
        except KeyError:
            raise Failure
        value = getter(key)
        setattr(self, key, value)
        return value

    @property
    def questions(cls):
        return cls.answers.keys()

class Knowledge(object):
    __metaclass__ = KnowledgeMeta