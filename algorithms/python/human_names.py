from lxml import html

class NamesMeta(type):
    SURNAME_URL = 'http://names.mongabay.com/most_common_surnames.htm'
    MALENAME_URL = 'http://names.mongabay.com/male_names.htm'
    FEMNAME_URL = 'http://names.mongabay.com/female_names.htm'

    @property
    def male_names(cls):
        try:
            return cls._male_names
        except AttributeError:
            cls._male_names = cls._list_of(cls.MALENAME_URL)
            return cls._male_names

    @staticmethod
    def _list_of(url):
        parser = html.parse(url)
        return [el.text for el in parser.xpath('//td/table/tr/td[1]')
                if el.text]


class Names(object):
    __metaclass__ = NamesMeta