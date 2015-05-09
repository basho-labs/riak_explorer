import Ember from 'ember';

export default Ember.Route.extend({
    model: function() {
        var serviceName = 'Riak Explorer';
        // var status = 'Unavailable';
        var pingUrl = '/explore/ping';

        return new Ember.RSVP.hash({
            service: serviceName,
            status: Ember.$.ajax({url: pingUrl, dataType: "json"}),
            routes: this.store.find('route')
        });

        // var ping = this.store.find('ping');
        // var routes = this.store.find('route');

        // console.log(ping.message);
        
        // if (ping && ping.message) {
            
        // }

        // return {
        //     service: serviceName,
        //     status: status,
        //     routes: routes
        // };
    }
});
