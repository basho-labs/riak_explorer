import Ember from 'ember';

function testData() {
    return {
        bucket_types: [
            {
                id:"default",
                props:{
                    active: true,
                    allow_mult: false,
                    basic_quorum: false,
                    big_vclock: 50,
                    chash_keyfun: "{riak_core_util,chash_std_keyfun}",
                    dvv_enabled: false,
                    dw: "quorum",
                    last_write_wins: false,
                    linkfun: "{modfun,riak_kv_wm_link_walker,mapreduce_linkfun}",
                    n_val: 3,
                    notfound_ok: true,
                    old_vclock: 86400,
                    postcommit: [],
                    pr: 0,
                    precommit: [],
                    pw: 0,
                    r: "quorum",
                    rw: "quorum",
                    small_vclock: 50,
                    w: "quorum",
                    write_once: false,
                    young_vclock: 20
                },
                links:{ self:"/explore/nodes/riak@127.0.0.1/bucket_types/default" }
            }
        ]
    };
}

export default Ember.Route.extend({
    queryParams: {
        node_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        // return testData();

        var result = this.store.find('bucket_type', { node_id: params.node_id });
        return result;
    }
});
