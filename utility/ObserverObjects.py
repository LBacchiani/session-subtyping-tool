class Observer:
    _observers = []

    def __init__(self):
        self._observers.append(self)
        self._observables = {}

    def observe(self, event_name, callback):
        self._observables[event_name] = callback

    @property
    def observers(self):
        return self._observers


class IOEvent:
    def __init__(self, location, text, filename, autofire=True):
        self.name = "IOEvent" + location
        self.text = text
        self.filename = filename
        if autofire: self.fire()

    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                observer._observables[self.name](self.text, self.filename)


class AlgSuccessEvent:
    def __init__(self, algconfig, autofire=True):
        self.name = "SuccessEvent"
        self.config = algconfig
        if autofire: self.fire()

    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                observer._observables[self.name](self.config)

class DualEvent:
    def __init__(self, location, type, autofire=True):
        self.name = "DualEvent" + location
        self.type = type
        if autofire: self.fire()

    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                observer._observables[self.name](self.type)

class ReloadConfigEvent:
    def __init__(self, newconfig, alg_name, autofire=True):
        self.name = "ReloadConfigEvent"
        self.newconfig = newconfig
        self.alg_name = alg_name
        if autofire: self.fire()

    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                observer._observables[self.name](self.newconfig, self.alg_name)


class FormatEvent:
    def __init__(self, new_format, autofire=True):
        self.name = "FormatEvent"
        self.format = new_format
        if autofire: self.fire()

    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                observer._observables[self.name](self.format)

