import Ember from 'ember';

export default Ember.Route.extend({
    model: function() {
        var serviceName = 'Riak Explorer';
        var pingUrl = '/explore/ping';
        var propsUrl = '/explore/props';

        return new Ember.RSVP.hash({
            service: serviceName,
            pingResult: Ember.$.ajax({url: pingUrl, dataType: "json"}),
            propsResult: Ember.$.ajax({url: propsUrl, dataType: "json"}),
            routes: this.store.find('route')
        });
    }
});
