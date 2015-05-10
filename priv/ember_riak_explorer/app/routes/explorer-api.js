import Ember from 'ember';

export default Ember.Route.extend({
    model: function() {
        var serviceName = 'Riak Explorer';
        var pingUrl = '/explore/ping';

        return new Ember.RSVP.hash({
            service: serviceName,
            status: Ember.$.ajax({url: pingUrl, dataType: "json"}),
            routes: this.store.find('route')
        });
    }
});
